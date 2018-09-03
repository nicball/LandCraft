module Server where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.Socket
import System.IO 

import Network

roomSize :: Int
roomSize = 2

startServer :: Integral a => a -> IO ()
startServer port
    = withSocketsDo
    . bracketOnError (socket AF_INET Stream defaultProtocol) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (0, 0, 0, 0)))
        listen sock 5
        hPutStrLn stderr "Listening..."
        forever $ do
            sessions <- newMVar Map.empty
            fini <- newEmptyMVar
            acceptN roomSize sock () $ \() hdl addr -> do
                hPutStrLn stderr ("Incoming connection: " ++ show addr ++ ".")
                forkIO $ do
                    Join name <- expect hdl isJoin
                    hPutStrLn stderr (name ++ " (" ++ show addr ++ ") connected.")
                    modifyMVar_ sessions $ \ss -> do
                        forM_ (Map.elems ss) $ \h -> (serialize h (Just name, Join name))
                        return (Map.insert name hdl ss)
                    putMVar fini ()
                return ()
            replicateM_ roomSize (takeMVar fini)
            ss <- takeMVar sessions
            forkFinally (startRoom ss) (\_ -> mapM_ hClose (Map.elems ss) >> putStrLn "Room closed.")

acceptN :: Int -> Socket -> a -> (a -> Handle -> SockAddr -> IO a) -> IO a
acceptN n sock init onConn = go n init
    where go n acc | n > 0 = withAcceptCloseOnError sock $ \hdl addr -> do
                                 acc' <- onConn acc hdl addr
                                 go (n - 1) acc'
                   | otherwise = return acc

withAcceptCloseOnError :: Socket -> (Handle -> SockAddr -> IO a) -> IO a
withAcceptCloseOnError sock f = do
    (clnt, addr) <- accept sock
    hdl <- socketToHandle clnt ReadWriteMode `onException` close clnt
    hSetBuffering hdl (BlockBuffering Nothing) `onException` hClose hdl
    f hdl addr `onException` hClose hdl

startRoom :: Map String Handle -> IO ()
startRoom sessions = do
    hPutStrLn stderr ("Room started with " ++ show (Map.keys sessions) ++ ".")
    forever . forM_ (Map.assocs sessions) $ \(name, hdl) -> do
        serialize hdl (Nothing :: Maybe String, Poll)
        cmd <- expect hdl isCommandOrNoCommand
        when (isCommand cmd) . forM_ (Map.assocs sessions) $ \(_, h) ->
            serialize h (Just name, cmd)
