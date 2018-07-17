module DivideBy where

data DivisionResult a = Success a | DivideByZero
    deriving (Show)

divideBy :: Integral a => a -> a -> DivisionResult a
divideBy x y
    | y == 0             = DivideByZero
    | (x < 0) /= (y < 0) = Success (divideNegative x y 0)
    | otherwise          = Success (dividePositive x y 0)
    where 
        divideNegative x' y' total = 
            if x' <= 0 then total 
            else divideNegative (x' + y') y' (total + 1)
        dividePositive x' y' total = 
            if x' < y' then total 
            else dividePositive (x' - y') y' (total + 1)

