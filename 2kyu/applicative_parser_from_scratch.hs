-- https://www.codewars.com/kata/54f1fdb7f29358dd1f00015d

module ApplicativeParser where

import Data.Char
import Data.Maybe
import Control.Monad
import Prelude hiding (fmap)

class Applicative f => Alternative f where
    -- | The identity of '<|>'
    empty :: f a
    -- | An associative binary operation
    (<|>) :: f a -> f a -> f a

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

instance Functor Parser where
  fmap f (P p) = P $ \xs ->
    [ (ys, f y) | (ys, y) <- p xs ]

instance Applicative Parser where
  pure = return
  (P pf) <*> (P px) = P $ \xs ->
    [(zs, f a) | (ys, f) <- pf xs, 
    (zs, a) <- px ys]

instance Monad Parser where
  return a = P $ \xs -> [(xs, a)]
  p >>= f  = P $ concatMap (\(a, xs) -> unP (f a) xs) . (swap <$>) . unP p
    where swap (a, b) = (b, a)

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
predP p = P $ \(x:xs) ->
  if p x then [(xs, x)]
         else [       ]

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
a <<>> b = P $ \xs -> case unP a xs of
  [] -> unP b xs
  ys -> ys

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many v = many_v
  where
    many_v = some_v <<>> inject []
    some_v = liftA2 (:) v many_v

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some v = some_v
  where
    many_v = some_v <<>> inject []
    some_v = liftA2 (:) v many_v

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

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 
parseExpr :: String -> Maybe Expr
parseExpr s = error "undefined"


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
