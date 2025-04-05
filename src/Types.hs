module Types where

type PID = Int
type Percent = Double
type MicroSecond = Int

data OrderDirection = OrderAsc
                    | OrderDec
    deriving (Eq)
