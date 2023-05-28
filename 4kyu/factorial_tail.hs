-- https://www.codewars.com/kata/55c4eb777e07c13528000021

import Data.List (genericLength, group)

factors :: (Integral a) => a -> [(a, a)]
factors = map (\xs -> (head xs, genericLength xs)) . group . f 2 []
  where f p ps n
          | n < p = ps
          | n `mod` p == 0 = f p (p:ps) (n `div` p)
          | otherwise = f (p+1) ps n

zeroes :: Integral a => a -> a -> a
zeroes b n = minimum . map f $ factors b
  where f (p, c) = (`div` c) . sum . map (n `div`) . takeWhile (<= n) . map (p^) $ [1..]

main = do
  print ((zeroes 10 10), 2)
  print ((zeroes 10 127), 31)
  print ((zeroes 16 16), 3)
  print ((zeroes 180 5721), 1427)
