{-# LANGUAGE OverloadedStrings, DataKinds #-}
module ProcessInformation
    ( fetchProcesses
    , orderProcesses
    , ProcessTime
    , ProcessInfo (..)
    , ProcessOrder (..)
    ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (sortOn, zip5)
import Data.Maybe (fromMaybe, isJust)

import Control.Monad.Freer (Eff, runM)
import Control.Monad.Freer.Error (Error, runError)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
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

fetchProcesses :: MicroSecond -> MaybeT IO [ProcessInfo]
fetchProcesses delay = do
    pids <- MaybeT $ runMaybe getPIDs
    maybeCPUUsage <- MaybeT $ runMaybe $ getProcessCPUUsages pids (fromIntegral delay)
    maybeNames <- liftIO $ mapM (((T.pack <$>) <$>) . runMaybe . getProcessName) pids
    maybeMemUsage <- MaybeT $ runMaybe $ getProcessMemUsages pids
    maybeCPUTimes <- MaybeT $ runMaybe $ getProcessTimes pids

    let maybeTimes = (mircoSeocondsToTime <$>) <$> maybeCPUTimes

        cpuUsage = fromMaybe 0 <$> maybeCPUUsage
        times = fromMaybe (mircoSeocondsToTime 0) <$> maybeTimes
        memUsage = fromMaybe 0 <$> maybeMemUsage
        names = fromMaybe "?" <$> maybeNames

        mask0 = isJust <$> maybeCPUUsage
        mask1 = isJust <$> maybeTimes
        mask2 = isJust <$> maybeNames
        mask3 = isJust <$> maybeMemUsage

        infos = createProcessInfo <$> zip5 pids cpuUsage memUsage times names
        infosFiltered = [info | (info, True, True, True, True) <- zip5 infos mask0 mask1 mask2 mask3]
    pure infosFiltered
    where
        eitherToMaybe :: Either e a -> Maybe a
        eitherToMaybe (Left _) = Nothing
        eitherToMaybe (Right a) = Just a
        runMaybe :: Eff '[Error String, IO] a -> IO (Maybe a)
        runMaybe eff = eitherToMaybe <$> runM (runError eff)
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
