module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool b1 b2
    | b1 == b2  = [b1]
    | otherwise = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ GT = [EQ, GT]
eftOrd o1 o2
    | o1 == o2  = [o1]
    | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt x y
    | x > y     = []
    | otherwise = x : eftInt (x + 1) y

eft :: (Enum a, Ord a) => a -> a -> [a]
eft x y
    | x > y     = []
    | otherwise = x : eft (succ x) y
