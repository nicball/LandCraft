module Util where

import Control.Concurrent.Chan
import Control.Monad
import System.Clock

data TimeoutChan a
    = TimeoutChan (Chan (a, TimeSpec))

newTimeoutChan :: IO (TimeoutChan a)
newTimeoutChan = TimeoutChan <$> newChan

writeTimeoutChan :: TimeoutChan a -> a -> IO ()
writeTimeoutChan (TimeoutChan ch) a = do
    now <- getTime Monotonic
    writeChan ch (a, now)

readTimeoutChan :: (Num d, Ord d) => TimeoutChan a -> Maybe d -> IO a
readTimeoutChan (TimeoutChan ch) (Just timeout) = do
    (a, t) <- readChan ch
    now <- getTime Monotonic
    let diff = fromIntegral . flip div 1000 . toNanoSecs . diffTimeSpec now $ t
    if diff > timeout
    then readTimeoutChan (TimeoutChan ch) (Just timeout)
    else return a
readTimeoutChan (TimeoutChan ch) Nothing
    = fst <$> readChan ch

while :: IO Bool -> IO ()
while action = do
    continue <- action
    when continue $ while action
