-- https://www.codewars.com/kata/56b2abae51646a143400001d

import Data.Bits (shiftR, xor)

mystery :: Int -> Int
mystery = xor <*> (`shiftR` 1)

mysteryInv :: Int -> Int
mysteryInv = foldr xor 0 . takeWhile (>0) . iterate (`shiftR` 1)

nameOfMystery :: [Char]
nameOfMystery = "Gray code"

main :: IO()
main = do
  print $ mystery 6 -- 5
  print $ mysteryInv 5 -- 6
  
  print $ mystery 9 -- 13
  print $ mysteryInv 13 -- 9
  
  print $ mystery 19 -- 26
  print $ mysteryInv 26 -- 19
