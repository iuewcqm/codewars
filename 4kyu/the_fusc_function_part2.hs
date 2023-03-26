-- https://www.codewars.com/kata/57040e445a726387a1001cf7

fusc :: Integer -> Integer
fusc n = f n 1 0
  where f n a b = case (divMod n 2) of
          (0, 0) -> b
          (0, 1) -> a + b
          (x, 0) -> f x (a+b) b
          (x, 1) -> f x a (a+b)

main :: IO()
main = do
  print $ fusc <$> [0..30]
  print $ fusc ((2^1000)+1)
  print $ fusc ((2^1000)-1)
