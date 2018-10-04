module Client where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Maybe
import Network.Socket
import System.IO

import Network
import Model
import Config
import Util
import Render

startClient :: String-> String -> String -> IO ()
startClient userName serverName serverPort
    = withSocketsDo . bracket openConn hClose $ startGame
    where openConn = do
              putStrLn ("Connecting " ++ serverName ++ ":" ++ serverPort ++ " ...")
              let hints = defaultHints { addrSocketType = Stream }
              addr : _ <- getAddrInfo (Just hints) (Just serverName) (Just serverPort)
              sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
              connect sock (addrAddress addr) `onException` close sock
              hdl <- socketToHandle sock ReadWriteMode
              hSetBuffering hdl (BlockBuffering Nothing)
              return hdl
          startGame hdl = do
              serialize hdl (Join userName)
              (Nothing, JoinResp joined) <- unserialize hdl
                  :: IO (Maybe String, Message)
              if joined
              then do putStrLn "Connected. Waiting for enough players."
                      gameLoop hdl
              else do putStrLn "Name already used. Exiting."
                      return ()
          gameLoop hdl = do
              game <- newMVar (emptyGameState mapSize)
              (terminal, inputChan) <- initUI game
              while $ do
                  msg <- unserialize hdl
                      :: IO (Maybe String, Message)
                  case msg of
                      (Nothing, Poll) -> do
                          gs <- readMVar game
                          case gsMyUid gs of
                              Just _ ->
                                  if amIAlive gs
                                  then do
                                      cmd <- readTimeoutChan inputChan (Just 200) 
                                      serialize hdl (Command cmd)
                                  else serialize hdl NoCommand
                              Nothing -> do
                                  coord <- genLocation gs
                                  serialize hdl (Command (WrappedCommand (Spawn coord)))
                      (Just nm, Command (WrappedCommand cmd)) -> do
                          modifyMVar_ game $ \gs ->
                              case cmd of
                                  Spawn _ | nm == userName ->
                                      let (uid, gs') = runCommand gs cmd
                                      in return  gs' { gsMyUid = Just uid }
                                  _ -> return . snd . runCommand gs $ cmd
                          redrawTerminal terminal
                      (Just _, Join name) ->
                          putStrLn (name ++ " joined the game.")
                      _ -> 
                          putStrLn ("Unknown command " ++ show msg)
                  gs <- readMVar game
                  if allDead gs && isJust (gsMyUid gs)
                  then do
                      putStrLn "Everyone died. Game over."
                      return False
                  else return True

while :: IO Bool -> IO ()
while action = do
    continue <- action
    when continue $ while action

printGame :: GameState -> IO ()
printGame gs = do
    let CoordSys size = gsCoordSys gs
    let tiles = genTiles gs
    forM_ [size - 1, size - 2 .. 0] $ \y -> do
        forM_ [0 .. size - 1] $ \x ->
            putStr (tiles x y)
        putChar '\n'
    putChar '\n'
    where genTiles gs = if amIAlive gs
                        then genView gs (fromJust . gsMyUid $ gs)
                        else if isNothing . gsMyUid $ gs
                        then genMist
                        else genAll gs
          genAll gs x y = case findAliveUnitByPos gs (x, y) of
              Just unit -> genUnit unit
              Nothing -> plainTile
          genView gs uid x y
              = if inSight gs uid (x, y)
                then case findAliveUnitByPos gs (x, y) of
                    Just unit -> genUnitMe (findUnitById gs uid) unit
                    Nothing -> plainTile
                else mistTile
          genMist x y = mistTile
          genUnit unit = withColor unit "U"
          genUnitMe me unit = withColor unit $
              if me == unit
              then "M"
              else "U"
          withColor unit str
              = let hp = unitHp unit
                    color = setSGRCode [SetColor Foreground Vivid $
                        if hp >= 5 then Green
                        else if hp >= 3 then Yellow
                        else Red]
                in color ++ str ++ setSGRCode []

initUI :: MVar GameState -> IO (Terminal, TimeoutChan WrappedCommand)
initUI game = do
    inputChan <- newTimeoutChan
    term <- createTerminal "LandCraft" (drawGame game) (parseKey game inputChan)
    forkIO startTerminals
    return (term, inputChan)

parseKey :: MVar GameState -> TimeoutChan WrappedCommand -> Char -> IO ()
parseKey game chan char = do
    uidm <- gsMyUid <$> readMVar game
    case uidm of
        Just uid -> case char of
            'w'-> writeTimeoutChan chan . WrappedCommand . Move uid $ UpD
            'a'-> writeTimeoutChan chan . WrappedCommand . Move uid $ LeftD
            's'-> writeTimeoutChan chan . WrappedCommand . Move uid $ DownD
            'd'-> writeTimeoutChan chan . WrappedCommand . Move uid $ RightD
            'q'-> writeTimeoutChan chan . WrappedCommand . Quit $ uid
            _ -> return ()
        Nothing -> return ()
