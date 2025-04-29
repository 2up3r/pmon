module Thread (updaterThread) where

import Control.Concurrent (MVar, forkIO, killThread, takeMVar, threadDelay)
import Control.Monad (when)
import Control.Monad.Fix (fix)

import Brick.BChan (BChan, writeBChan)
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)

import Types (MicroSecond)

updaterThread :: BChan a -> MVar MicroSecond -> (MicroSecond -> IO a) -> IO ()
updaterThread chan updateVar updater = do
    initDelay <- takeMVar updateVar
    initThreadId <- forkIO $ updaterThread' initDelay
    _ <- fix (\loop oldThreadId -> do
        delay <- takeMVar updateVar
        killThread oldThreadId
        newThreadId <- forkIO $ updaterThread' delay
        loop newThreadId
        ) initThreadId
    pure ()
    where
        updaterThread' :: MicroSecond -> IO ()
        updaterThread' microSec = fix $ \loop -> do
            start <- toNanoSecs <$> getTime Monotonic
            result <- updater microSec
            end <- toNanoSecs <$> getTime Monotonic
            let elapsed = fromIntegral $ end - start
                remaining = microSec - elapsed `div` 1000
            when (remaining > 0) $ threadDelay remaining
            writeBChan chan result
            loop
