module Datatypes where

import Data.List

data Mood = Blah | Woot deriving (Show, Eq)

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

settleDown x = if x == Woot then Blah else x

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome s = s == reverse s

myAbs :: (Ord a, Num a) => a -> a
myAbs n = if n > 0 then n
          else (-n)

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

type Name = String

functionC :: (Ord a) => a -> a -> Bool
functionC x y = 
    if x > y then True else False

data Container a = Container a
instance Eq a => Eq (Container a) where
    Container a == Container a' = a == a'

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn a') = a == a'

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt = 
    TisAnInt Int | 
    TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt a') = a == a'
    (==) (TisAString a) (TisAString a') = a == a'
    (==) _ _ = False

data Pair a = Pair a a
    deriving Show
instance Eq a => Eq (Pair a) where
    (==) (Pair a b) (Pair a' b') = a == a' && b == b'

divideThenAdd :: (Fractional a) => a -> a -> a
divideThenAdd x y = (x / y) + 1

data DaysOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Eq, Ord, Enum, Bounded)

young :: Ord a => [a] -> a
young xs = head (sort xs)

check :: Eq b => (a -> b) -> a -> b -> Bool
check f a b = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b -> b
arith f i a b = fromInteger i + f a + b
