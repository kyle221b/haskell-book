module Exercises where

itIsMystery xs = map isVowel xs
    where isVowel x = x `elem` "aeiou"
