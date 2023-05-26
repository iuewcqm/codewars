-- https://www.codewars.com/kata/58ad317d1541651a740000c5

import Data.List (sort)

middlePermutation :: String -> String
middlePermutation s | even (length s) = e $ sort s
                    | otherwise       = o $ sort s

e s = y : (reverse $ xs++ys)
  where (xs, y:ys) = splitAt mid s
        mid = length s `div` 2 - 1

o s = y : (e $ xs++ys)
  where (xs, y:ys) = splitAt mid s
        mid = length s `div` 2 

main :: IO()
main = do
 print $ (middlePermutation "abc", "bac")
 print $ (middlePermutation "pxjunoagmtdskylbfzchvqirew", "mzyxvutsqponlkjhgfdcba")
