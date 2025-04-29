module Main (main) where

import Brick (customMain)
import Brick.BChan (newBChan)
import GHC.Conc (forkIO, killThread)
import Graphics.Vty (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)

import TUI (MyEvent(PSUpdate), initialDelay, mkApp, mkInitialState)
import Thread (updaterThread)
import ProcessInformation (fetchProcesses)
import Control.Concurrent (newMVar)

main :: IO ()
main = do
    putStrLn "Running ..."

    eventChan <- newBChan 10
    updaterVar <- newMVar initialDelay
    threadId <- forkIO $ updaterThread eventChan updaterVar ((PSUpdate <$>) <$> fetchProcesses)
    let initialState = mkInitialState updaterVar
    let buildVty = mkVty defaultConfig
    initialVty <- buildVty
    _ <- customMain initialVty buildVty (Just eventChan) mkApp initialState
    killThread threadId

    putStrLn "Done"
