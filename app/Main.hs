module Main (main) where

import Control.Concurrent (newMVar)
import Data.Maybe (fromMaybe)
import GHC.Conc (forkIO, killThread)

import Brick (customMain)
import Brick.BChan (newBChan)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Graphics.Vty (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)

import ProcessInformation (fetchProcesses)
import Thread (updaterThread)
import TUI (MyEvent(PSUpdate), initialDelay, mkApp, mkInitialState)

main :: IO ()
main = do
    eventChan <- newBChan 10
    updaterVar <- newMVar initialDelay
    let psUpdater x = PSUpdate . fromMaybe [] <$> runMaybeT (fetchProcesses x)
        initialState = mkInitialState updaterVar
        buildVty = mkVty defaultConfig
    threadId <- forkIO $ updaterThread eventChan updaterVar psUpdater
    initialVty <- buildVty
    _ <- customMain initialVty buildVty (Just eventChan) mkApp initialState
    killThread threadId
