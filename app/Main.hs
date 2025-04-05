module Main (main) where

import Brick (customMain)
import Brick.BChan (newBChan)
import Control.Concurrent (newMVar, killThread)
import GHC.Conc (forkIO)
import Graphics.Vty (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)

import TUI (MyEvent(PSUpdate), mkApp, mkInitialState, initialDelay)
import Thread (updaterThread)
import ProcessInformation (fetchProcesses)

defaultUpdateStep :: Int
defaultUpdateStep = 10000

main :: IO ()
main = do
    putStrLn "Running ..."

    eventChan <- newBChan 10
    delayVar <- newMVar initialDelay
    updaterId <- forkIO $ updaterThread eventChan delayVar defaultUpdateStep $ PSUpdate <$> fetchProcesses

    let buildVty = mkVty defaultConfig
    initialVty <- buildVty
    _ <- customMain initialVty buildVty (Just eventChan) mkApp (mkInitialState delayVar)

    killThread updaterId
    
    putStrLn "Done"
