module Network where

import Control.Arrow
import Data.Word
import System.IO
import qualified Data.ByteString.Lazy as BS
import GHC.Generics (Generic)
import Data.Binary (Binary, encode, decode)
import Model

data Message
    = Join String
    | Command WrappedCommand
    | Poll
    | Quit String
    deriving (Generic, Show)
instance Binary Message

serializeIO :: Binary a => Handle -> a -> IO ()
serializeIO handle a = do
    let bs = encode a
    let len = fromIntegral . BS.length $ bs :: Word16
    BS.hPut handle . encode $ len
    BS.hPut handle bs

unserializeIO :: Binary a => Handle -> IO a
unserializeIO handle = do
    lenBs <- BS.hGet handle 2
    let len = fromIntegral (decode lenBs :: Word16)
    bs <- BS.hGet handle len
    return (decode bs)
