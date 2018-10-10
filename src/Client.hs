module Client where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Maybe
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Network.Socket
import System.IO

import Config
import Model
import Network
import Render
import Util

data Resources = Resources
    { resGameBoardImg :: Image
    , resPlainImg :: Image
    , resMistImg :: Image
    , resUnitImg :: Image
    }

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
            connected <- withConnectionFork userName serverName serverPort $
                gameLogic gameState
                          userCmdChan
                          postEmptyEvent
                          (windowShouldClose win True)
            when connected . withDrawer $ \drawer ->
                withResources $ \res -> do
                    clearColor $= Color4 0 0 0 0
                    setKeyCallback win . Just $ onKeyboard gameState userCmdChan
                    mainLoop win drawer res gameState
    where mainLoop win drawer res gameState = fix $ \loop -> do
              clear [ColorBuffer]
              gs <- readMVar gameState
              drawGame drawer res gs
              swapBuffers win
              waitEvents
              closep <- windowShouldClose win
              if closep 
              then putStrLn "exiting"
              else loop

withConnectionFork :: String -> String -> String -> (Handle -> IO ()) -> IO Bool
withConnectionFork userName serverName serverPort action = do
    putStrLn ("Connecting " ++ serverName ++ ":" ++ serverPort ++ " ...")
    let hints = defaultHints { addrSocketType = Stream }
    addr : _ <- getAddrInfo (Just hints) (Just serverName) (Just serverPort)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr) `onException` close sock
    socketToHandle sock ReadWriteMode `bracketOnError` hClose $ \hdl ->
        hSetBuffering hdl (BlockBuffering Nothing)
        serialize hdl (Join userName)
        (Nothing, JoinResp joined)
            <- unserialize hdl :: IO (Maybe String, Message)
        if joined
        then do putStrLn "Connected. Waiting for enough players."
                forkFinally (const $ hClose hdl) (action hdl)
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
                gs <- readMVar gameState
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
                modifyMVar_ gameState $ \gs ->
                    case cmd of
                        Spawn _ | nm == userName ->
                            let (uid, gs') = runCommand gs cmd
                            in return  gs' { gsMyUid = Just uid }
                        _ -> return $ execCommand gs cmd
                updateScreen
                when (isQuit cmd) . putStrLn $ nm ++ " quited."
            (Just _, Join name) ->
                putStrLn $ name ++ " joined the gameState."
            _ -> 
                putStrLn $ "Unknown command " ++ show msg
        gs <- readMVar gameState
        if allDead gs && isJust (gsMyUid gs)
        then do
            putStrLn "Everyone died. Game over."
            gameOver
            return False
        else return True

drawGame :: Drawer -> Resources -> GameState -> IO ()
drawGame drawer res gameState = do
    drawGameBoard
    forM_ allCellCoords $ \(x, y) ->
        if amIAlive gameState
        then if inSight (fromJust $ gsMyUid gs) (x, y)
             then drawCell x y
             else drawMist x y
        else if amIWatcher gameState
        then drawCell x y
        else drawMist x y
    drawGridLines
    where drawGameBoard
              = drawImage drawer (resGameBoardImg res) (-1, -1) 2 2
          drawGridLines =
              let coords = concat [[(i, 0), (i, mapSize), (0, i), (mapSize, i)] |
                                  i <- [1 .. mapSize - 1]]
                  vcs = map (\(x, y) -> ( ( fromIntegral x * cellSize - 1
                                          , fromIntegral y * cellSize - 1
                                          )
                                        , (1, 1, 1, 1)
                                        ))
                            coords
              in drawColor drawer Lines vcs
          drawCell x y
              = case findAliveUnitByPos gameState (x, y) of
                  Just unit -> drawUnit drawer unit (resUnitImg res) (cellPos x y) cellSize
                  Nothing -> drawPlain x y
          drawMist x y
              = drawImage drawer (resMistImg res) (cellPos x y) cellSize cellSize
          drawPlain x y
              = drawImage drawer (resPlainImg res) (cellPos x y) cellSize cellSize
          CoordSys mapSize = gsCoordSys gameState
          cellSize = 2 / fromIntegral mapSize
          cellPos x y = (x * cellSize - 1, y * cellSize - 1)
          allCellCoords = [(x, y) | let a = [0 .. mapSize - 1], x <- a, y <- a]

drawUnit :: Drawer -> Unit -> Image -> (GLfloat, GLfloat) -> GLfloat -> IO ()
drawUnit drawer unit img pos@(x, y) cellSize = do
    drawImage drawer img pos cellSize (cellSize * 0.8)
    let origin = (x, y + cellSize * 0.8)
        width = cellSize * fromIntegral (unitHp unit)
                         / fromIntegral (unitHp defaultUnit)
        height = cellSize * 0.2
        color = calcColor (unitHp unit)
        vcs = [ ((x        , y         ), color)
              , ((x        , y + height), color)
              , ((x + width, y         ), color)
              , ((x        , y + height), color)
              , ((x + width, y + height), color)
              , ((x + width, y         ), color)
              ]
    drawColor drawer Triangles vcs
    where calcColor hp | hp >= 5 = (0, 1, 0, 1)
                       | hp >= 3 = (1, 1, 0, 1)
                       otherwise = (1, 0, 0, 1)  

onKeyboard :: Window
           -> MVar GameState
           -> TimeoutChan WrappedCommand
           -> Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
onKeyboard win gameState userCmdChan evwin key _ keyState mods = do
    if not (win == evwin && keyState == KeyState'Pressed && noMods)
    then return ()
    else do
        uidm <- gsMyUid <$> readMVar gameState
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
