-- https://www.codewars.com/kata/52b7ed099cdc285c300001cd

module Main where

import Data.List (sort)

sumOfIntervals :: [(Int, Int)] -> Int
sumOfIntervals = sumOfSortedIntervals . sort
  where sumOfSortedIntervals []      = 0
        sumOfSortedIntervals [(x,y)] = y-x
        sumOfSortedIntervals ((x1,x2):(y1,y2):xs)
          | x2 > y1   = sumOfIntervals ((x1, max x2 y2):xs)
          | otherwise = x2-x1 + sumOfIntervals ((y1,y2):xs)

main :: IO()
main = do
  print $ sumOfIntervals [(1,2),(6,10),(11,15)]
  print $ sumOfIntervals [(1,5),(10,20),(1,6),(16,19),(5,11)]
  print $ sumOfIntervals [(0,20),(-100000000,10), (30,40)]
