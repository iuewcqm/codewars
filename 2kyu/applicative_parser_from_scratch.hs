-- https://www.codewars.com/kata/54f1fdb7f29358dd1f00015d

module ApplicativeParser where

import Data.Char
import Control.Monad
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

instance Functor Parser where
  fmap f (P p) = P $ \xs ->
    [ (ys, f y) | (ys, y) <- p xs ]

instance Applicative Parser where
  pure = return
  (P pf) <*> (P px) = P $ \xs ->
    [(zs, f a) | (ys, f) <- pf xs, (zs, a) <- px ys]

instance Monad Parser where
  return a = P $ \xs -> [(xs, a)]
  p >>= f  = P $ concatMap (\(a, xs) -> unP (f a) xs) . (swap <$>) . unP p
    where swap (a, b) = (b, a)

(<|>) :: Parser a -> Parser a -> Parser a 
a <|> b = P $ \xs -> case unP a xs of
    [] -> unP b xs
    ys -> ys

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap = (<$>)

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) a = (const a <#>)

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP f = P g
  where
    g "" = []
    g (x:xs)
      | f x       = [(xs, x)]
      | otherwise = []

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject = return

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.

liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
liftA2 f x = (<*>) (pmap f x)

(<@>) :: Parser (a -> b) -> Parser a -> Parser b
(<@>) = (<*>)

(<@) :: Parser a -> Parser b -> Parser a
(<@) = (<*)

(@>) :: Parser a -> Parser b -> Parser b
(@>) = (*>)

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP []     = return []
stringP (x:xs) = do
  charP   x
  stringP xs
  return (x:xs)
  
-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
a <<>> b = P $ \xs -> unP a xs ++ unP b xs

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many v = many_v
  where
    many_v = some_v <|> inject []
    some_v = (:) <$> v <*> many_v

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some v = some_v
  where
    many_v = some_v <|> inject []
    some_v = (:) <$> v <*> many_v

-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = case unP p cs of
  [] -> []
  xs -> [ o | (x, o) <- xs, x == [] ]

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case runParser p cs of
  [a] -> Just a
  _   -> Nothing

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  a <- p
  rest a
  where rest a = do
          f <- op
          b <- p
          rest $ f a b
          <|> return a

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op = (chainl1 p op <|>) . return

nat :: Parser Int
nat    = read <$> some digit

oneOf :: String -> Parser Char
oneOf  = predP . flip elem

digit :: Parser Char
digit  = predP isDigit

spaces :: Parser String
spaces = many $ oneOf " \n\r\t"

number :: Parser Int
number = do
  x  <- stringP "-" <|> return []
  xs <- some digit
  return $ read $ x ++ xs

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 

exprP :: Parser Expr
exprP = constP <|> binOpExprP <|> negP <|> zeroP

constP :: Parser Expr
constP = do
  n <- number
  return $ ConstE n

binOpExprP :: Parser Expr
binOpExprP = do
  stringP "("
  e1 <- exprP
  stringP " "
  op <- binOpP
  stringP " "
  e2 <- exprP
  stringP ")"
  return $ BinOpE op e1 e2

binOpP :: Parser BinOp
binOpP = (stringP "+" >> return AddBO) <|> (stringP "*" >> return MulBO)

negP :: Parser Expr
negP = do
  stringP "-"
  e <- exprP
  return $ NegE e

zeroP :: Parser Expr
zeroP = do
  stringP "z"
  return ZeroE

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE x)         = x
evalExpr (BinOpE AddBO x y) = (evalExpr x) + (evalExpr y)
evalExpr (BinOpE MulBO x y) = (evalExpr x) * (evalExpr y)
evalExpr (NegE x)           = negate $ evalExpr x
evalExpr (ZeroE)            = 0

parseExpr :: String -> Maybe Expr
parseExpr s = case runParser exprP s of
  (h:_) -> Just h
  _     -> Nothing


-- tests
shouldBe :: Eq a => a -> a -> Bool
shouldBe = (==)

main :: IO()
main = do
    print "running parsers"
    let ambigP   = unambigP <<>> unambigP
        unambigP = charP 'a'

    print $ "runParserUnique should not allow multiple results"
    print $ runParserUnique ambigP "a" `shouldBe` Nothing

    print $ "runParserUnique should work with a single result"
    print $ runParserUnique unambigP "a" `shouldBe` Just 'a'
     
    print $ "runParser should allow multiple results"
    print $ runParser ambigP "a" `shouldBe` ['a', 'a']

    print $ "runParser should work with a single result"
    print $ runParser unambigP "a" `shouldBe` ['a']

    print "expressions"
    let interpretExpr = fmap evalExpr . parseExpr
    print "--1"
    print $ interpretExpr "--1" `shouldBe` Just 1

    print "(z + -z)"
    print $ interpretExpr "(z + -z)" `shouldBe` Just 0

    print "(2 + -1)"
    print $ interpretExpr "(2 + -1)" `shouldBe` Just 1

    print "((-(4 * 2) * z) + (2 + 3))"
    print $ interpretExpr "((-(4 * 2) * z) + (2 + 3))" `shouldBe` Just 5

    print "invalid binary op"
    print $ interpretExpr "(z / 1)" `shouldBe` Nothing

    print "needs parenthesis"
    print $ interpretExpr "z + 1" `shouldBe` Nothing

    print "only one whitespace as sep"
    print $ interpretExpr "(1 +  1)" `shouldBe` Nothing

    print $ interpretExpr "zz" `shouldBe` Nothing
