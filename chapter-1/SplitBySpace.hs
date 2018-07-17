module SplitBySpace where

split :: Char -> String -> [String]
split _ "" = []
split delim s = token : split delim rest
    where f     = (/= delim)
          token = takeWhile f s
          rest  = snd . splitAt 1 . dropWhile f $ s

splitBySpace :: String -> [String]
splitBySpace = split ' '

splitLines :: String -> [String]
splitLines = split '\n'

firstSen = "Tyger Tyger, burning bright\n" 
secondSen = "In the forests of the night\n" 
thirdSen = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual =
       [ "Tyger Tyger, burning bright"
       , "In the forests of the night"
       , "What immortal hand or eye"
       , "Could frame thy fearful symmetry?"
       ]

main :: IO ()
main = print $
       "Are they equal? " ++ show (splitLines sentences == shouldEqual)

