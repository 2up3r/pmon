module CircularBuffer
    ( CircularBuffer
    , newCBuffer
    , pushCBuffer
    , readCBuffer
    ) where

data CircularBuffer a = CircularBuffer Int [a]
    deriving (Eq)

instance Functor CircularBuffer where
    fmap f (CircularBuffer idx buf) = CircularBuffer idx $ f <$> buf

instance Foldable CircularBuffer where
    foldr f z = foldr f z . readCBuffer

newCBuffer :: [a] -> CircularBuffer a
newCBuffer = CircularBuffer 0

pushCBuffer :: a -> CircularBuffer a -> CircularBuffer a
pushCBuffer val (CircularBuffer idx buf) = CircularBuffer ((idx + 1) `mod` length buf) $ replaceAt idx val buf
    where
        replaceAt :: Int -> a -> [a] -> [a]
        replaceAt ri nv l = (\(i,v) -> if i == ri then nv else v) <$> zip [0 .. length l - 1] l

readCBuffer :: CircularBuffer a -> [a]
readCBuffer (CircularBuffer idx buf) = let (before, after) = splitAt idx buf
                                       in after ++ before
