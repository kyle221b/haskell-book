module StandardFunctions where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = 
    if x == True then True
    else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = 
    if a == x then True
    else myElem a xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = myAny (== x)

squish :: [[a]] -> [a]
squish [] = []
squish ([]:xs) = squish xs
squish ((x:xs):xss) = x : squish (xs : xss)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:y:xs) = myMaximumBy f (best:xs)
    where best = if f x y == GT then x else y

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare
