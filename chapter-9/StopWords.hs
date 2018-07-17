module StopVowelComb where

import Data.List

stop = "pbtdkg"
vowels = "aeiou"

combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine f xs ys = concat . map combineSingle $ xs
    where combineSingle x' = foldr (\a b -> f x' a : b) [] ys

threeTuple :: [a] -> [a] -> [a] -> [(a, a, a)]
threeTuple xs ys zs = combine g xs . combine (,) ys $ zs
    where g x (y, z) = (x, y, z)

main :: IO ()
main = do 
    putStrLn $ unwords . map toString . filter prefixCondition $ combinations
        where prefixCondition (x, _, _) = x == 'p'
              toString (x, y, z) = [x, y, z]
              combinations = threeTuple stop vowels stop

