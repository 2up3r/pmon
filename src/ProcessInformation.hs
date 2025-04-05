{-# LANGUAGE OverloadedStrings #-}
module ProcessInformation 
    ( fetchProcesses
    , orderProcesses
    , ProcessTime
    , ProcessInfo (..)
    , ProcessOrder (..)
    ) where

import Data.List (sortOn)
-- import System.Info (os)
import System.Process (readProcess)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

import Types (PID, Percent, OrderDirection (..))

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

---------- display ----------

orderProcesses :: ProcessOrder -> OrderDirection -> [ProcessInfo] -> [ProcessInfo]
orderProcesses OrderPID     d = applyOrderDirection d . sortOn piPID
orderProcesses OrderCPU     d = applyOrderDirection d . sortOn piCPUPercent
orderProcesses OrderMemory  d = applyOrderDirection d . sortOn piMemoryPercent
orderProcesses OrderTime    d = applyOrderDirection d . sortOn piProcessTime
orderProcesses OrderCommand d = applyOrderDirection d . sortOn piProcessCommand

applyOrderDirection :: OrderDirection -> [a] -> [a]
applyOrderDirection OrderAsc = id
applyOrderDirection OrderDec = reverse

---------- FETCHERS ----------

fetchProcesses :: IO [ProcessInfo]
fetchProcesses = fetchPsProcess
-- fetchProcesses = case os of
--     "darwin" -> fetchDarwinProcesses
--     "linux" -> fetchLinuxProcesses
--     _ -> pure []

---------- DARWIN ----------

-- fetchDarwinProcesses :: IO [ProcessInfo]
-- fetchDarwinProcesses = undefined

---------- LINUX ----------

-- fetchLinuxProcesses :: IO [ProcessInfo]
-- fetchLinuxProcesses = undefined

---------- PS ----------

fetchPsProcess :: IO [ProcessInfo]
fetchPsProcess = do
    r <- readProcess "ps" ["-axm", "-o pid,%cpu,%mem,time,comm"] []
    let ps = A.parse pPs $ T.pack r
    case ps of
        (A.Done _ ps') -> pure ps'
        (A.Partial p) -> do
            case p "" of
                (A.Done _ ps') -> pure ps'
                _ -> pure []
        _ -> pure []

pLine :: A.Parser T.Text
pLine = A.takeTill A.isEndOfLine

pInt :: A.Parser Int
pInt = read <$> A.many' A.digit

pTime :: A.Parser ProcessTime
pTime = ProcessTime 
    <$> pInt
    <*> (":" *> pInt)
    <*> ("." *> pInt)

pProcess :: A.Parser ProcessInfo
pProcess = ProcessInfo
    <$> (A.many' A.space *> pInt)
    <*> (A.many' A.space *> A.double)
    <*> (A.many' A.space *> A.double)
    <*> (A.many' A.space *> pTime)
    <*> (A.many' A.space *> pLine)

pProcesses :: A.Parser [ProcessInfo]
pProcesses = A.sepBy pProcess A.endOfLine

pPs :: A.Parser [ProcessInfo]
pPs = pLine *> A.endOfLine
   *> pProcesses
