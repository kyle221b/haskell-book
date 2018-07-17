module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar n = map (shiftChar n)

uncaesar :: Int -> String -> String
uncaesar n = caesarShift (26 - n)

shiftChar :: Int -> Char -> Char
shiftChar n c
    | ord c < ord 'Z' = shift 'A'
    | otherwise       = shift 'a'
    where shift base = chr $ ord base + mod (ord c - ord base + n) 26
