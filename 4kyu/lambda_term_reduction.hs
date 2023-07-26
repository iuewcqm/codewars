-- https://www.codewars.com/kata/55f307bef98d9c4adc000006

module LambdaTermReduction (lambda,free,bound,reduce) where

import Data.List (union, (\\))

data Lambda = Var String
            | Abs String Lambda
            | App Lambda Lambda
        
instance Show Lambda where
  show (Var s)   = s
  show (Abs s l) = s ++ " " ++ show l ++ " λ"
  show (App l r) = show l ++ " " ++ show r ++ " @"

lambda :: String -> Lambda
lambda = head . lambda' [] . words
  where
    lambda' ts           [      ] = ts
    lambda' (t:Var s:ts) ("λ":xs) = lambda' (Abs s t:ts) xs
    lambda' (r:l:ts)     ("@":xs) = lambda' (App l r:ts) xs
    lambda' ts           ( x :xs) = lambda' (Var x:ts) xs

free :: Lambda -> [String]
free (Var s)   = [s]
free (Abs s l) = free l \\ [s]
free (App l r) = union (free l) (free r)

bound :: Lambda -> [String]
bound (Var _)   = []
bound (Abs s l) = union [s] (bound l)
bound (App l r) = union (bound l) (bound r)

reduce :: Lambda -> Lambda
reduce (Var s)   = Var s
reduce (Abs s l) = Abs s (reduce l)
reduce (App l r) = case l' of
       (Abs s t) -> reduce $ substitute t s r'
       _         -> App l' r'
  where
    l' = reduce l
    r' = reduce r

substitute :: Lambda -> String -> Lambda -> Lambda
substitute (Var n)   v t = if n == v then t else Var n
substitute (Abs n l) v t = Abs n $ if n /= v then (substitute l v t) else l
substitute (App l r) v t = App (substitute l v t) (substitute r v t)
            
-- tests
shouldBe :: Eq a => a -> a -> Bool
shouldBe = (==)

main = do
  let term = lambda "x"
  print $ show term `shouldBe` "x"
  print $ free term `shouldBe` [ "x" ]
  print $ bound term `shouldBe` []
  print $ show (reduce term) `shouldBe` "x"
  let term = lambda "x x λ y @"
  print $ show term `shouldBe` "x x λ y @"
  print $ free term `shouldBe` [ "y" ]
  print $ bound term `shouldBe` [ "x" ]
  print $ show (reduce term) `shouldBe` "y"
