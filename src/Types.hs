{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Types where

type PID = Int
type Percent = Double
type MicroSecond = Int

data OrderDirection = OrderAsc
                    | OrderDec
    deriving (Eq)

newtype CircularBuffer a = CircularBuffer [a]
    deriving (Eq, Functor, Foldable)

pushCBuffer :: a -> CircularBuffer a -> CircularBuffer a
pushCBuffer _ (CircularBuffer []) = CircularBuffer []
pushCBuffer y (CircularBuffer xs) = CircularBuffer $ tail xs ++ [y]
