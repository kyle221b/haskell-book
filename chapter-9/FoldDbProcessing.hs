module DBProcessing where

import Data.Maybe
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [
    DbDate (UTCTime
        (fromGregorian 1911 5 1)
        (secondsToDiffTime 34123)), 
    DbNumber 9001, 
    DbString "Hello, world!", 
    DbDate (UTCTime
        (fromGregorian 1921 5 1)
        (secondsToDiffTime 34123))
    ]

filterDbNum = filterDb num

filterDbDate = filterDb date

filterDb :: (DatabaseItem -> Maybe a) -> [DatabaseItem] -> [a]
filterDb f = foldr (\a b -> (fromMaybe b) . (fmap (:b)) . f $ a) []

filterDb' :: (DatabaseItem -> Maybe a) -> [DatabaseItem] -> [a]
filterDb' f = mapMaybe f

mostRecent :: [DatabaseItem] -> Maybe UTCTime
mostRecent = foldr (compareValues . date) Nothing
    where compareValues (Just t1) (Just t2) = Just (max t1 t2)
          compareValues a Nothing = a
          compareValues Nothing a = a

date :: DatabaseItem -> Maybe UTCTime
date (DbDate time) = Just time
date _             = Nothing

num :: DatabaseItem -> Maybe Integer
num (DbNumber number) = Just number
num _                 = Nothing

avgDb :: [DatabaseItem] -> Maybe Double
avgDb xs = if count == 0 then Nothing else Just (total / count)
    where (total, count)      = foldr step (0, 0) xs
          step a tup          = fromMaybe tup . fmap (addElement tup . fromIntegral) . num $ a
          addElement (t, c) x = (x + t, c + 1)

newtype Plus a = Plus a
    deriving Show
instance Num a => Monoid (Plus a) where
    mempty = Plus 0
    Plus x `mappend` Plus y = Plus (x + y)

avg :: Fractional a => [a] -> a
avg xs = value total / value count
    where (total, count) =  mconcat . map (,Plus 1) . map Plus $ xs
          value (Plus a) = a











