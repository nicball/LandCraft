module Server where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Map.Strict as Map
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
        sessions <- newMVar Map.empty
        forever . acceptAsync sock $ \hdl addr -> do
            hPutStrLn stderr ("Incoming connection: " ++ show addr ++ ".")
            Join name <- expect hdl isJoin
            hPutStrLn stderr (name ++ " (" ++ show addr ++ ") connected.")
            (joined, complete) <- modifyMVar sessions $ \ss -> do
                if Map.member name ss
                then return (ss, (False, Nothing))
                else let ss' = Map.insert name hdl ss
                     in if Map.size ss' == roomSize
                        then return (Map.empty, (True, Just ss'))
                        else return (ss', (True, Nothing))
            serialize hdl (Nothing :: Maybe String, JoinResp joined)
            case complete of
                Just ss -> void . forkFinally (startRoom ss) $
                    (\_ -> mapM_ hClose (Map.elems ss)
                        >> hPutStrLn stderr "Room closed.")
                Nothing -> return ()

acceptAsync :: Socket -> (Handle -> SockAddr -> IO ()) -> IO ()
acceptAsync sock f = do
    (clnt, addr) <- accept sock
    hdl <- socketToHandle clnt ReadWriteMode `onException` close clnt
    hSetBuffering hdl (BlockBuffering Nothing) `onException` hClose hdl
    void . forkIO $ f hdl addr `onException` hClose hdl

startRoom :: Map String Handle -> IO ()
startRoom sessions = do
    hPutStrLn stderr ("Room started with " ++ show (Map.keys sessions) ++ ".")
    forever . forM_ (Map.assocs sessions) $ \(name, hdl) -> do
        serialize hdl (Nothing :: Maybe String, Poll)
        cmd <- expect hdl isCommandOrNoCommand
        when (isCommand cmd) . forM_ (Map.assocs sessions) $ \(_, h) ->
            serialize h (Just name, cmd)
