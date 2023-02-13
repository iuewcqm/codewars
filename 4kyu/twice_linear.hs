import Data.List (sort, nub)

dblLinear :: Int -> [Integer]
dblLinear n = take (2*n) $ u y z

y = [2*x+1 | x <- [1..]]
z = [3*x+1 | x <- [1..]]
u yss@(y:ys) zss@(z:zs)
  | y == z    = y: u ys zs
  | y < z     = y: u ys zss
  | otherwise = z: u yss zs 


main :: IO()
main = do
  print $ dblLinear 10
