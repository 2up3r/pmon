module Thread (updaterThread) where

import Brick.BChan (BChan, writeBChan)
import Control.Concurrent (threadDelay, MVar, readMVar)
import Control.Monad (when)
import Control.Monad.Fix (fix)

import Types (MicroSecond)

updaterThread :: BChan a -> MVar MicroSecond -> MicroSecond -> IO a -> IO ()
updaterThread chan delayVar delayStep op = fix $ \loop -> do
    a <- op
    writeBChan chan a
    delayUpdateable delayStep delayVar
    loop

delayUpdateable :: Int -> MVar Int -> IO ()
delayUpdateable turn delayVar = do
    d <- readMVar delayVar
    delayUpdateable' d d (min turn d) delayVar

delayUpdateable' :: Int -> Int -> Int -> MVar Int -> IO ()
delayUpdateable' 0 _ _ _ = pure ()
delayUpdateable' _ 0 _ _ = pure ()
delayUpdateable' _ _ 0 _ = pure ()
delayUpdateable' current original step mvar = do
    d <- readMVar mvar
    when (d == original) $ do
        threadDelay step
        let remain = max 0 (current - step)
        if remain < step
            then delayUpdateable' remain original remain mvar
            else delayUpdateable' remain original step mvar
