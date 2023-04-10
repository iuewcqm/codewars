-- https://www.codewars.com/kata/599a0d02755eae7070000079

import Control.Applicative
import Data.Char

data Token = Token String | Brackets [Token] deriving (Eq, Show)
data Parser a = Parser (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser p) input = p input
operatorChars :: String
operatorChars = "!#$%&*+-/<=>@^_.,;"

instance Functor Parser where
  fmap f p = Parser $ \input -> do
    (x, xs) <- parse p input
    Just (f x, xs)

instance Applicative Parser where
  pure x  = Parser $ \input -> Just (x, input)
  p <*> q = Parser $ \input -> do
    (f, input' ) <- parse p input
    (x, input'') <- parse q input'
    Just (f x, input'')

instance Alternative Parser where
  empty   = Parser $ \input -> Nothing
  p <|> q = Parser $ \input -> parse p input <|> parse q input

charP :: Char -> Parser Char
charP c = Parser f
  where f "" = Nothing
        f (x:xs)
          | x == c    = Just (x, xs)
          | otherwise = Nothing

predP :: (Char -> Bool) -> Parser Char
predP f = Parser g
  where g [] = Nothing
        g (x:xs)
          | f x       = Just (x, xs)
          | otherwise = Nothing

spanP :: (Char -> Bool) -> Parser String
spanP f = some $ predP f

ws :: Parser String
ws = many $ predP isSpace

operators :: Parser Token
operators = Token <$> (ws *> spanP (`elem` operatorChars) <* ws)

string :: Parser Token
string = Token <$> (ws *> spanP (`notElem` " ()"++operatorChars) <* ws)

token :: Parser Token
token = operators <|> string <|> brackets

brackets :: Parser Token
brackets = Brackets <$> (ws *> charP '(' *> many token <* charP ')' <* ws)

tokenize :: String -> Maybe [Token]
tokenize "" = Just []
tokenize xs = case parse token xs of
      Nothing            -> Nothing
      Just (token, rest) -> (token :) <$> tokenize rest

main :: IO()
main = do
  print $ tokenize "A + B * C"
  print $ tokenize "function a(arg1, arg2)"
  print $ tokenize "(1(2)1)"
  print $ tokenize ", () +&"
  print $ tokenize "a b@c"
  print $ tokenize "do $ readLine >>= putStrLn"
  print $ tokenize "Mismatched bracket )" -- Nothing
