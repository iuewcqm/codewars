-- https://www.codewars.com/kata/541af676b589989aed0009e7

import Data.List (sort)

countChange :: Integer -> [Integer] -> Integer
countChange num xs = sum $ map (tree 0) coins
  where coins = (reverse . sort) xs
        memo f = map (\x -> map (f x) [0..]) [0..]
        lookupTable = memo tree
        lookupH acc val = lookupTable !! (fromIntegral acc) !! (fromIntegral val)
        tree acc val
          | acc + val >  num = 0
          | acc + val == num = 1
          | otherwise        = sum $ map (lookupH $ acc + val) $ dropWhile (>val) coins

main :: IO()
main = do
  print $ countChange 4  [2, 1] -- 3
  print $ countChange 10 [3, 5, 2] -- 4
  print $ countChange 11 [5, 7] -- 0
  print $ countChange 10 [3, 4, 2, 1] -- 23
  print $ countChange 300 [500,5,50,100,20,200,10] -- 1022
  print $ countChange 3000 [500,5,50,100,20,200,10] -- 22481738
  print $ countChange 30000 [500,5,50,100,20,200,10] -- 11055689599346
