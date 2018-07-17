module WordNumber where

import Data.List (intersperse)

digitToWord :: Integral a => a -> String
digitToWord n = case n of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    0 -> "zero"

digits :: Integral a => a -> [a]
digits = reverse . digitsHelper
    where digitsHelper 0 = []
          digitsHelper n = rem : digitsHelper quot
                where (quot, rem) = divMod n 10

wordNumber :: Integral a => a -> String
wordNumber n = unwords . intersperse "-" . (map digitToWord) . digits $ n
