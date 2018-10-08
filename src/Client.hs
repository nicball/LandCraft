module Client where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Maybe
import Graphics.UI.GLFW
import Network.Socket
import System.IO

import Config
import Model
import Network
import Render
import Util

startClient :: String-> String -> String -> IO ()
startClient userName serverName serverPort = withSocketsDo $ do
    gameState <- newMVar (emptyGameState mapSize)
    userCmdChan <- newTimeoutChan
    withGLFW $ do
        ver <- getVersionString
        case ver of
            Just str -> putStrLn str
            Nothing -> return ()
        withWindow 600 600 "Land Craft" $ \win -> do
            connected <- withConnection userName serverName serverPort $
                forkIO . gameLogic gameState
                                   userCmdChan
                                   postEmptyEvent
                                   (windowShouldClose win True)
            when connected $ do
                drawer <- createDrawer
                clearColor $= Color4 0 0 0 0
                setKeyCallback win . Just $ onKeyboard gameState userCmdChan
                mainLoop win drawer gameState
    where mainLoop win drawer gameState = do
              clear [ColorBuffer]
              gs <- readMVar gameState
              drawGame drawer gs
              swapBuffers win
              waitEvents
              closep <- windowShouldClose win
              if closep 
              then putStrLn "exiting"
              else mainLoop win drawer

withConnection :: String -> String -> String -> (Handle -> IO a) -> IO a
withConnection userName serverName serverPort action = do
    putStrLn ("Connecting " ++ serverName ++ ":" ++ serverPort ++ " ...")
    let hints = defaultHints { addrSocketType = Stream }
    addr : _ <- getAddrInfo (Just hints) (Just serverName) (Just serverPort)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr) `onException` close sock
    socketToHandle sock ReadWriteMode `bracketOnError` hClose $ \hdl ->
        hSetBuffering hdl (BlockBuffering Nothing)
        serialize hdl (Join userName)
        (Nothing, JoinResp joined) <- unserialize hdl
            :: IO (Maybe String, Message)
        if joined
        then do putStrLn "Connected. Waiting for enough players."
                action hdl
                return True
        else do putStrLn "Name already used. Exiting."
                return False

gameLogic :: MVar GameState
          -> TimeoutChan WrappedCommand
          -> IO ()
          -> IO ()
          -> Handle
          -> IO ()
gameLogic gameState userCmdChan updateScreen gameOver conn
    = while $ do
        msg <- unserialize conn :: IO (Maybe String, Message)
        case msg of
            (Nothing, Poll) -> do
                gs <- readMVar game
                case gsMyUid gs of
                    Just _ ->
                        if amIAlive gs
                        then do
                            cmd <- readTimeoutChan inputChan (Just 200) 
                            serialize conn (Command cmd)
                        else serialize conn NoCommand
                    Nothing -> do
                        coord <- genLocation gs
                        serialize conn (Command (WrappedCommand (Spawn coord)))
            (Just nm, Command (WrappedCommand cmd)) -> do
                modifyMVar_ game $ \gs ->
                    case cmd of
                        Spawn _ | nm == userName ->
                            let (uid, gs') = runCommand gs cmd
                            in return  gs' { gsMyUid = Just uid }
                        _ -> return . snd . runCommand gs $ cmd
                updateScreen
            (Just _, Join name) ->
                putStrLn (name ++ " joined the game.")
            _ -> 
                putStrLn ("Unknown command " ++ show msg)
        gs <- readMVar game
        if allDead gs && isJust (gsMyUid gs)
        then do
            putStrLn "Everyone died. Game over."
            gameOver
            return False
        else return True

drawGame :: Drawer -> GameState -> IO ()
drawGame drawer gameState = do
    let CoordSys size = gsCoordSys gameState
    drawCells
    if amIAlive gameState
    then drawView (fromJust . gsMyUid $ gameState)
    else if isNothing . gsMyUid $ gameState
    then drawMist
    else drawAll
    where drawAll x y = case findAliveUnitByPos gameState (x, y) of
              Just unit -> drawUnit unit
              Nothing -> plainTile
          drawView uid x y
              = if inSight gameState uid (x, y)
                then case findAliveUnitByPos gameState (x, y) of
                    Just unit -> drawUnitMe (findUnitById gameState uid) unit
                    Nothing -> plainTile
                else mistTile
          drawMist x y = mistTile
          drawUnit unit = withColor unit "U"
          drawUnitMe me unit = withColor unit $
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

onKeyboard :: Window
           -> MVar GameState
           -> TimeoutChan WrappedCommand
           -> Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
onKeyboard win gameState userCmdChan evwin key _ keyState mods = do
    if not (win == evwin && keyState == KeyState'Pressed && noMods)
    then return ()
    else do
        uidm <- gsMyUid <$> readMVar game
        case uidm of
            Just uid -> case key of
                Key'W -> writeTimeoutChan userCmdChan . WrappedCommand . Move uid $ UpD
                Key'A -> writeTimeoutChan userCmdChan . WrappedCommand . Move uid $ LeftD
                Key'S -> writeTimeoutChan userCmdChan . WrappedCommand . Move uid $ DownD
                Key'D -> writeTimeoutChan userCmdChan . WrappedCommand . Move uid $ RightD
                Key'Q -> writeTimeoutChan userCmdChan . WrappedCommand . Quit $ uid
                _ -> return ()
            Nothing -> return ()
    where moMods = mods == ModifierKeys False False False False
