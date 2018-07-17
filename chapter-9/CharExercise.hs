module CharExercise where

import Data.Char

removeLower :: String -> String
removeLower = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (s : xs) = toUpper s : xs

capitalizeFull :: String -> String
capitalizeFull [] = []
capitalizeFull (s : xs) = toUpper s : capitalizeFull xs

capitalizeAndExtractFirst :: String -> Maybe Char
capitalizeAndExtractFirst s = case s of
    []        -> Nothing
    otherwise -> Just $ toUpper . head $ s
