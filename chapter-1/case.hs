module Cases where

functionC :: Ord a => a -> a -> a
functionC a b = 
    case x of
        True -> a
        False -> b
    where x = a > b

nums :: (Ord a, Num a, Num b) => a -> b
nums x = 
    case compare x 0 of
        GT -> 1
        LT -> -1
        EQ -> 0

instance Monoid Integer where
    mempty = 0
    mappend x y = x + y

nums2 :: (Ord a, Monoid a, Num b) => a -> b
nums2 x
    | x > zero  = 1
    | x < zero  = -1
    | otherwise = 0
    where zero = mempty x

dodgy x y = x + y * 10
f = dodgy 2 3
g = (flip dodgy) 2 3

mc91 :: Integral a => a -> a
mc91 x
    | x > 100   = x - 10
    | otherwise = mc91 . mc91 $ x + 11
