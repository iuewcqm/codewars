module Fixit where

import Prelude hiding (reverse, foldr)

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' _ []     = []
reverse' f (a:as) = f as ++ [a]

foldr' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' _ _ b []     = b
foldr' f g b (a:as) = a `g` f g b as

-- tests
fix :: (a -> a) -> a
fix f = let x = f x in x

reverse = fix reverse'
foldr   = fix foldr'

main :: IO()
main = do
  print $ reverse "some_test"
  
  print $ foldr (+) 0 [1,2,3]
  print $ take 6 $ foldr (\a b -> (2 * a):b) [] [0..]
