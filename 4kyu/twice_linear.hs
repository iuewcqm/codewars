dblLinear :: Int -> Integer
dblLinear = (u !!)

y = [2*x+1 | x <- u]
z = [3*x+1 | x <- u]
u = 1 : u' y z
  where u' yss@(y:ys) zss@(z:zs)
          | y > z     = z : u' yss zs
          | y < z     = y : u' ys zss
          | otherwise = y : u' ys zs

main :: IO()
main = do
  print $ dblLinear 10 -- 22
  print $ dblLinear 20 -- 57
  print $ dblLinear 30 -- 91
  print $ dblLinear 50 -- 175
