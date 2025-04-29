{-# LANGUAGE OverloadedStrings, DataKinds #-}
module ProcessInformation
    ( fetchProcesses
    , orderProcesses
    , ProcessTime
    , ProcessInfo (..)
    , ProcessOrder (..)
    ) where

import Control.Concurrent (threadDelay)
import Data.List (sortOn, zip5)
import Data.Maybe (fromMaybe)

import Control.Monad.Freer (runM, Eff)
import Control.Monad.Freer.Error (runError, Error)
import PsInfo
import qualified PsInfo.Util.Types as PI
import qualified Data.Text as T

import Types (PID, Percent, OrderDirection (..), MicroSecond)

data ProcessTime = ProcessTime Int Int Int
    deriving (Eq, Ord)

instance Show ProcessTime where
    show (ProcessTime h m s) = pad ' ' 3 h <> ":" <> pad '0' 2 m <> "." <> pad '0' 2 s where
        pad :: Char -> Int -> Int -> String
        pad p l n = let str = show n
                        len = max l (length str)
                     in reverse $ take len $ reverse str <> repeat p

data ProcessInfo = ProcessInfo
    { piPID :: PID
    , piCPUPercent :: Percent
    , piMemoryPercent :: Percent
    , piProcessTime :: ProcessTime
    , piProcessCommand :: T.Text
    }
    deriving (Eq)

data ProcessOrder = OrderPID
                  | OrderCPU
                  | OrderMemory
                  | OrderTime
                  | OrderCommand
    deriving (Eq)

orderProcesses :: ProcessOrder -> OrderDirection -> [ProcessInfo] -> [ProcessInfo]
orderProcesses OrderPID     d = applyOrderDirection d . sortOn piPID
orderProcesses OrderCPU     d = applyOrderDirection d . sortOn piCPUPercent
orderProcesses OrderMemory  d = applyOrderDirection d . sortOn piMemoryPercent
orderProcesses OrderTime    d = applyOrderDirection d . sortOn piProcessTime
orderProcesses OrderCommand d = applyOrderDirection d . sortOn piProcessCommand

applyOrderDirection :: OrderDirection -> [a] -> [a]
applyOrderDirection OrderAsc = id
applyOrderDirection OrderDec = reverse

fetchProcesses :: MicroSecond -> IO [ProcessInfo]
fetchProcesses delay = do
    epids <- runM $ runError getPIDs :: IO (Either String [PI.PID])
    case epids of
        (Left _) -> pure []
        (Right pids) -> do
            times0 <- mapM (runMaybe . getProcessTime) pids
            ewall0 <- runM $ runError getWallTime :: IO (Either String PI.MicroSecond)
            case ewall0 of
                (Left _) -> pure []
                (Right wall0) -> do
                    threadDelay delay
                    times1 <- mapM (runMaybe . getProcessTime) pids
                    ewall1 <- runM $ runError getWallTime :: IO (Either String PI.MicroSecond)
                    case ewall1 of
                        (Left _) -> pure []
                        (Right wall1) -> do
                            let maybeDeltaTimes = (\(t1,t0) -> (-) <$> t1 <*> t0) <$> zip times1 times0
                                deltaTimes = fromMaybe 0 <$> maybeDeltaTimes
                                times = mircoSeocondsToTime . fromMaybe 0 <$> times1
                                deltaWall = wall1 - wall0
                                cpuUsage = (/ fromIntegral deltaWall) . fromIntegral <$> deltaTimes
                            maybeMemUsage <- mapM (runMaybe . getProcessMemUsage) pids
                            let memUsage = fromMaybe 0 <$> maybeMemUsage
                            maybeNames <- mapM (runMaybe . getProcessName) pids
                            let names = maybe "?" T.pack <$> maybeNames
                            epids' <- runM $ runError getPIDs :: IO (Either String [PI.PID])
                            case epids' of
                                (Left _) -> pure []
                                (Right pids') -> do
                                    let infos = createProcessInfo <$> zip5 pids cpuUsage memUsage times names
                                        infosFiltered = filter ((`elem` pids') . PI.PID . piPID) infos
                                    pure infosFiltered
    where
        runMaybe :: Eff '[Error String, IO] a -> IO (Maybe a)
        runMaybe eff = do
            ev <- runM $ runError eff
            case ev of
                (Left _) -> pure Nothing
                (Right v) -> pure $ Just v
        mircoSeocondsToTime :: PI.MicroSecond -> ProcessTime
        mircoSeocondsToTime mcs = 
            let secs = mcs `div` 1000000 
                mins = secs `div` 60
                hrs = mins `div` 60
            in ProcessTime 
                (fromIntegral hrs) 
                (fromIntegral $ mins `mod` 60) 
                (fromIntegral $ secs `mod` 60)
        createProcessInfo :: (PI.PID, Percent, Percent, ProcessTime, T.Text) -> ProcessInfo
        createProcessInfo (PI.PID pid, cpu, mem, time, comm) = 
            ProcessInfo pid cpu mem time comm
