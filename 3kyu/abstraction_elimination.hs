-- https://www.codewars.com/kata/601c6f43bee795000d950ed1

{-# Options_GHC -O0 #-}

module AbstractionElimination (eliminate) where

import Data.List (union, (\\))
import Text.Parsec
import Text.Parsec.String

-- "λxy.x"      -> K
-- "λxy.y"      -> KI
-- "λxyz.x(yz)" -> S(KS)K

-- λx.x        = I
-- λxy.x       = K
-- λxyz.xz(yz) = S

-- Abs x (Abs y (App y x))
-- λx.λy.(y x)
-- λxy.yx

-- λxyz.xz(yz) = S
-- Abs x (Abs y (Abs z
--     (App (App x z) (App y z))
--       )      )

data Term = Var String
          | Abs String Term
          | App Term   Term
          | S | K | I

instance Show Term where
  show S = "S"
  show K = "K"
  show I = "I"
  show (App S t) = "S" ++ show t
  show (App K I) = "KI"
  show (App K K) = "KK"
  show (App K t) = "K(" ++ show t ++ ")"
  show (App I t) = "I" ++ show t
  show (App (Var l) (Var r)) = "(" ++ l ++ r ++ ")"
  show (App l I) = "(" ++ show l ++ ")I"
  show (App l K) = "(" ++ show l ++ ")K"
  show (App l r) = show l ++ show r
  show (Var s) = s
  show (Abs s t) = "λ" ++ s ++ "." ++ show t

transform :: Term -> Term
transform S = S
transform K = K
transform I = I
transform (Var s) = Var s -- rule 1
transform (App l r) = App (transform l) (transform r) -- rule 2
transform (Abs x (App l (Var y))) | x == y && (not $ free x l) = transform l -- η-reduction
transform (Abs x (Var y))         | x == y               = I -- rule 4
transform (Abs x term@(Abs y t))  | free x t             = transform $ Abs x (transform term) -- rule 5
transform (Abs x (App l r))       | free x l || free x r = App S $ App (transform $ Abs x l) (transform $ Abs x r) -- 6
transform (Abs x t)               | not $ free x t       = App K (transform t) -- rule 3

free :: String -> Term -> Bool
free x S         = False
free x K         = False
free x I         = False
free x (Var s)   = s == x
free x (App l r) = (free x l) || (free x r)
free x (Abs s l) | x == s    = False
                 | otherwise = free x l

function :: Parser Term
function = char '(' *> lambdaExpr <* char ')'

lambdaExpr :: Parser Term
lambdaExpr = do char 'λ'
                input <- many1 letter
                char '.'
                expr <- term
                return $ Abs input expr
         <|> do apps <- many1 function
                return $ foldl1 App apps

term :: Parser Term
term = lambdaExpr <|> function

lambda :: String -> Term
lambda = undefined

eliminate :: String -> String
eliminate = show . transform . lambda


-- tests
shouldBe :: Eq a => a -> a -> Bool
shouldBe = (==)

shouldSatisfy :: String -> (String -> Bool) -> Bool
shouldSatisfy = flip ($)

validSKI :: String -> Bool
validSKI term = go 0 term
  where
    go n _ | n < 0 = errorWithoutStackTrace $ "unbalanced parentheses in term " ++ show term
    go 0 [] = True
    go _ [] = errorWithoutStackTrace $ "unbalanced parentheses in term " ++ show term
    go _ ('(':')':_) = errorWithoutStackTrace $ "empty application in term " ++ show term
    go n ('S':xs) = go n xs
    go n ('K':xs) = go n xs
    go n ('I':xs) = go n xs
    go n ('(':xs) = go (n+1) xs
    go n (')':xs) = go (n-1) xs
    go _ (x:_) = errorWithoutStackTrace $ "illegal character in term: " ++ show x

main :: IO()
main = do
  print "bluebird"
  let bluebird = eliminate "λxyz.x(yz)"
  print $ bluebird `shouldSatisfy` validSKI
  print $ bluebird `shouldBe` "S(KS)K"

  print "cardinal"
  let cardinal = eliminate "λxyz.xzy"
  print $ cardinal `shouldSatisfy` validSKI
  print $ cardinal `shouldBe` "S(S(KS)(S(KK)S))(KK)"

  print "identity"
  let identity = eliminate "λx.x"
  print $ identity `shouldSatisfy` validSKI
  print $ identity `shouldBe` "SKI"

  print "kestrel"
  let kestrel = eliminate "λxy.x"
  print $ kestrel `shouldSatisfy` validSKI
  print $ kestrel `shouldBe` "K"

  print "kite"
  let kite = eliminate "λxy.y"
  print $ kite `shouldSatisfy` validSKI
  print $ kite `shouldBe` "KI"

  print "mockingbird"
  let mockingbird = eliminate "λx.xx"
  print $ mockingbird `shouldSatisfy` validSKI
  print $ mockingbird `shouldBe` "SII"

  print "starling"
  let starling = eliminate "λxyz.xz(yz)"
  print $ starling `shouldSatisfy` validSKI
  print $ starling `shouldBe` "KISS"

  print "thrush"
  let thrush = eliminate "λxy.yx"
  print $ thrush `shouldSatisfy` validSKI
  print $ thrush `shouldBe` "S(K(SI))K"

  print "warbler"
  let warbler = eliminate "λxy.xyy"
  print $ warbler `shouldSatisfy` validSKI
  print $ warbler `shouldBe` "SS(KI)"

  print "why"
  let why = eliminate "λf.(λx.f(λy.xxy))(λx.f(λy.xxy))"
  print $ why `shouldSatisfy` validSKI
  print $ why `shouldBe` "S(S(S(KS)K)(K(SII)))(S(S(KS)K)(K(SII)))"
