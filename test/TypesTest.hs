{-# LANGUAGE DataKinds #-}
module TypesTest (tests) where

import Data.Foldable (Foldable(toList))

import Test.QuickCheck

import Types

propEqId :: [Int] -> Bool
propEqId xs = CircularBuffer xs == CircularBuffer xs

propSinglePush :: [Int] -> Int -> Bool
propSinglePush [] _ = True
propSinglePush xs y = let ys = tail xs ++ [y]
                        in CircularBuffer ys == pushCBuffer y (CircularBuffer xs)

propFoldableEq :: [Int] -> Bool
propFoldableEq xs = xs == toList (CircularBuffer xs)

propFmapNegate :: [Int] -> Bool
propFmapNegate xs = CircularBuffer (negate <$> xs) == (negate <$> CircularBuffer xs)

propReplaceAll :: [(Int, Int)] -> Bool
propReplaceAll as = let (xs, ys) = unzip as 
                    in CircularBuffer ys == foldl (flip pushCBuffer) (CircularBuffer xs) ys

propSinglePushFold :: [Int] -> Int -> Bool
propSinglePushFold [] _ = True
propSinglePushFold xs y = let ys = tail xs ++ [y]
                          in ys == toList (pushCBuffer y (CircularBuffer xs))

propFoldSum :: [Int] -> Bool
propFoldSum xs = sum xs == sum (CircularBuffer xs)

propFoldNegateFmapSum :: [Int] -> Bool
propFoldNegateFmapSum xs = sum (negate <$> xs) == sum (negate <$> CircularBuffer xs)

tests :: IO ()
tests = do
    quickCheck propEqId
    quickCheck propSinglePush
    quickCheck propFoldableEq
    quickCheck propFmapNegate
    quickCheck propReplaceAll
    quickCheck propSinglePushFold
    quickCheck propFoldSum
    quickCheck propFoldNegateFmapSum
