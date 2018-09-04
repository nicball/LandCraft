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
    | JoinResp Bool
    | Command WrappedCommand
    | Poll
    | NoCommand
    deriving (Generic, Show)
instance Binary Message

isJoin :: Message -> Bool
isJoin (Join _) = True
isJoin _ = False

isCommand :: Message -> Bool
isCommand (Command _) = True
isCommand _ = False

isCommandOrNoCommand :: Message -> Bool
isCommandOrNoCommand (Command _) = True
isCommandOrNoCommand NoCommand = True
isCommandOrNoCommand _ = False

expect :: Binary a => Handle -> (a -> Bool) -> IO a
expect hdl pred = do
    msg <- unserialize hdl
    if pred msg then return msg
    else expect hdl pred

serialize :: Binary a => Handle -> a -> IO ()
serialize handle a = do
    let bs = encode a
    let len = fromIntegral . BS.length $ bs :: Word16
    BS.hPut handle . encode $ len
    BS.hPut handle bs
    hFlush handle

unserialize :: Binary a => Handle -> IO a
unserialize handle = do
    lenBs <- BS.hGet handle 2
    let len = fromIntegral (decode lenBs :: Word16)
    bs <- BS.hGet handle len
    return (decode bs)
