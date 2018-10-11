module Client where

import qualified Codec.Picture as Juicy
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Function
import Data.Maybe
import qualified Data.Vector.Storable as Vector
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW hiding (Image)
import Network.Socket
import System.IO

import Config
import Model
import Network
import Render
import Util

data Resources = Resources
    { resPlainImg :: Image
    , resMistImg :: Image
    , resUnitImg :: Image
    }

data ResourceException = LoadException String deriving Show
instance Exception ResourceException

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
                          userName
                          userCmdChan
                          postEmptyEvent
                          (setWindowShouldClose win True)
            when connected . withDrawer $ \drawer ->
                withResources $ \res -> do
                    clearColor $= Color4 0 0 0 0
                    setKeyCallback win . Just $
                        onKeyboard win gameState userCmdChan
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
    socketToHandle sock ReadWriteMode `bracketOnError` hClose $ \hdl -> do
        hSetBuffering hdl (BlockBuffering Nothing)
        serialize hdl (Join userName)
        (Nothing, JoinResp joined)
            <- unserialize hdl :: IO (Maybe String, Message)
        if joined
        then do putStrLn "Connected. Waiting for enough players."
                forkFinally (action hdl) (const $ hClose hdl)
                return True
        else do putStrLn "Name already used. Exiting."
                return False

gameLogic :: MVar GameState
          -> String
          -> TimeoutChan WrappedCommand
          -> IO ()
          -> IO ()
          -> Handle
          -> IO ()
gameLogic gameState userName userCmdChan updateScreen gameOver conn
    = flip finally gameOver . while $ do
        msg <- unserialize conn :: IO (Maybe String, Message)
        case msg of
            (Nothing, Poll) -> do
                gs <- readMVar gameState
                case gsMyUid gs of
                    Just _ ->
                        if amIAlive gs
                        then do
                            cmd <- readTimeoutChan userCmdChan (Just 1000) 
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
            (Just _, Join name) -> do
                updateScreen
                putStrLn $ name ++ " joined the game."
            _ -> 
                putStrLn $ "Unknown command " ++ show msg
        gs <- readMVar gameState
        if allDead gs && isJust (gsMyUid gs)
        then do
            putStrLn "Everyone died. Game over."
            return False
        else return True

drawGame :: Drawer -> Resources -> GameState -> IO ()
drawGame drawer res gameState = do
    forM_ allCellCoords $ \(x, y) ->
        if amIAlive gameState
        then if inSight gameState (fromJust $ gsMyUid gameState) (x, y)
             then drawCell x y
             else drawMist x y
        else if amIWatcher gameState
        then drawCell x y
        else drawMist x y
    drawGridLines
    where drawGridLines =
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
          cellPos x y = ( fromIntegral x * cellSize - 1
                        , fromIntegral y * cellSize - 1
                        )
          allCellCoords = [(x, y) | let a = [0 .. mapSize - 1], x <- a, y <- a]

drawUnit :: Drawer -> Unit -> Image -> (GLfloat, GLfloat) -> GLfloat -> IO ()
drawUnit drawer unit img pos@(x, y) cellSize = do
    drawImage drawer img pos cellSize (cellSize * 0.8)
    let (ox, oy) = (x, y + cellSize * 0.8)
        width = cellSize * fromIntegral (unitHp unit)
                         / fromIntegral (unitHp $ defaultUnit (0, 0))
        height = cellSize * 0.2
        color = calcColor (unitHp unit)
        vcs = [ ((ox        , oy         ), color)
              , ((ox        , oy + height), color)
              , ((ox + width, oy         ), color)
              , ((ox        , oy + height), color)
              , ((ox + width, oy + height), color)
              , ((ox + width, oy         ), color)
              ]
    drawColor drawer Triangles vcs
    where calcColor hp | hp >= 5 = (0, 1, 0, 1)
                       | hp >= 3 = (1, 1, 0, 1)
                       | otherwise = (1, 0, 0, 1)  

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
    where noMods = mods == ModifierKeys False False False False

withResources :: (Resources -> IO a) -> IO a
withResources action = do
    withPicture plainImgPath $ \plImg ->
        withPicture mistImgPath $ \miImg ->
        withPicture unitImgPath $ \unImg ->
            action (Resources plImg miImg unImg)
    where withPicture path action = do
              img <- Juicy.readImage path
              rgba8@(Juicy.Image width height _) <- case img of
                  Left errstr -> throwIO $ LoadException errstr
                  Right dynImg -> return $ Juicy.convertRGBA8 dynImg
              let pixels = pic2pixels rgba8 width height
              withImage width height pixels action
          pic2pixels pic width height
              = [ u2f (r, g, b, a)
                | y <- [height - 1, height - 2 .. 0]
                , x <- [0 .. width - 1]
                , let Juicy.PixelRGBA8 r g b a = Juicy.pixelAt pic x y
                ]
          u2f (r, g, b, a)
              = ( fromIntegral r / 255
                , fromIntegral g / 255
                , fromIntegral b / 255
                , fromIntegral a / 255
                )
