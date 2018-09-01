module Server where

import System.IO (stderr)
import Text.Printf (hPrintf)
import Network.Socket

startServer :: Integral a => Server -> a -> IO ()
startServer port = withSocketsDo $ do
    bracket (socket AF_INET Stream defaultProtocol) close' $ \sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (0, 0, 0, 0)))
        listen sock 2
        withAcceptance sock $ \hdl1 addr1 -> do
            hPrintf stderr "Incoming connection: %s.\n" addr1
            withAcceptance sock $ \hdl2 addr2 -> do
                hPrintf stderr "Incoming connection: %s.\n" addr2
                serve hdl1 hdl2

withAcceptance :: Socket -> (Handle -> SockAddr -> IO a) -> IO a
withAcceptance sock
    = bracket acceptHdl (hClose . fst)
    where acceptHdl = do
              (conn, addr) <- accept sock
              hdl <- socketToHandle conn ReadWriteMode `onException` close conn
              return (hdl, addr)

serve :: Handle -> Handle -> IO ()
serve hdl1 hdl2 = do
    
