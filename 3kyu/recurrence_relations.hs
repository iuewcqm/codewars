-- https://www.codewars.com/kata/550756a881b8bdba99000348

module FunctionEvaluator where

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f n = case f n of
              Left b          -> b
              Right (args, g) -> g $ map (evaluateFunction f) args

-- tests
factorial i | i == 0    = Left 1
            | otherwise = Right ([i-1], (*i).head)

fibonacci i | i < 2     = Left i
            | otherwise = Right ([i-1, i-2], sum)
       
coinchange (a, i) | a == 0          = Left 1
                  | a < 0 || i == 0 = Left 0
                  | otherwise       = Right ([(a, i-1), (a-coinlist!!(i-1), i)], sum)
coinlist = [1, 3, 5, 10]

heigth (n, m) | m <= 0 || n <= 0 = Left 0
              | otherwise        = Right ([(n, m-1), (n-1, m-1)], (+1).sum)

foo  i | i <= 2    = Left 1
       | odd i     = Right ([6*i`div`7, 2*i`div`3], sum)
       | otherwise = Right ([i-1, i-3], sum)

main :: IO()
main = do
  print (evaluateFunction factorial 5, 120)
  print (evaluateFunction fibonacci 10, 55)
  print (evaluateFunction coinchange(20, length coinlist), 28)
  print (evaluateFunction heigth (2, 14), 105)
  print (evaluateFunction foo 20, 253)
