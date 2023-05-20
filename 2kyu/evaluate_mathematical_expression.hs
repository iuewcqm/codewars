-- https://www.codewars.com/kata/52a78825cdfc2cfc87000005

import Data.Char
import Control.Applicative

newtype Parser a = Parser
  {
    parse :: String -> Maybe (a, String)
  }

instance Functor Parser where
  fmap f p = Parser $ \input -> do
    (x, input') <- parse p input
    Just (f x, input')

instance Applicative Parser where
  pure x  = Parser $ \input -> Just (x, input)
  p <*> q = Parser $ \input -> do
    (f, input' ) <- parse p input
    (x, input'') <- parse q input'
    Just (f x, input'')

instance Alternative Parser where
  empty   = Parser $ \input -> Nothing
  p <|> q = Parser $ \input -> parse p input <|> parse q input

instance Monad Parser where
  p >>= f = Parser $ \input -> do
    (x, input') <- parse p input
    parse (f x) input'

charP :: Char -> Parser Char
charP c = Parser f
  where f [] = Nothing
        f (x:xs)
          | x == c    = Just (x, xs)
          | otherwise = Nothing

predP :: (Char -> Bool) -> Parser Char
predP f = Parser g
  where g [] = Nothing
        g (x:xs)
          | f x       = Just (x, xs)
          | otherwise = Nothing

string :: String -> Parser String
string = traverse charP

spaces :: Parser String
spaces = many $ predP isSpace

strip :: Parser a -> Parser a
strip p = spaces *> p <* spaces

digit :: Parser Char
digit = predP isDigit

minus :: Parser String
minus = some $ predP (=='-') <* spaces

positive :: Parser String
positive = some digit

negative :: Parser String
negative = do minus  <- charP '-'
              number <- positive
              return (minus:number)

int :: Parser String
int = positive <|> negative

float :: Parser String
float = do integer <- int
           divider <- charP '.'
           natural <- positive
           return (integer ++ divider:natural)

number :: Parser Double
number = double <$> (strip $ float <|> int)
  where double ds = read ds :: Double

expression :: Parser Double
expression = do x <- spaces *> term 
                loop x
         <|> term
  where 
    addSuffix t1 = do charP '+' <* spaces
                      y <- term 
                      loop (t1+y)
               <|> do m <- minus
                      y <- term
                      loop (if even $ length m then t1+y else t1-y)
    loop t1 = addSuffix t1 <|> return t1

term :: Parser Double
term = do x <- factor
          loop x
   <|> factor
   where
    addSuffix t1 = do charP '*' <* spaces
                      y <- factor
                      loop (t1*y)
               <|> do charP '/' <* spaces
                      y <- factor
                      loop (t1/y)
    loop t1 = addSuffix t1 <|> return t1
   
factor :: Parser Double
factor = do charP '(' <* spaces
            x <- expression 
            charP ')' <* spaces
            return x
     <|> do m <- minus 
            charP '(' <* spaces
            x <- expression
            charP ')' <* spaces
            return (if even $ length m then x else (-x))
     <|> do m <- minus
            x <- number
            return (if even $ length m then x else (-x))
     <|> number

calc :: String -> Double
calc xs = case parse expression xs of
               Nothing     -> 0
               Just (x, _) -> x


-- tests
test :: (String, Double) -> Either Double Bool
test (expr, answ)
  | actual == answ = Right True
  | otherwise      = Left  actual
  where actual = calc expr

main :: IO ()
main = do
  print $ test ("2 /2+3 * 4.75- -6", 21.25)
  print $ test ("2/(2+3)*4.33--6", 7.732)
  print $ test ("2 / (2 + 3) * 4.33 - -6", 7.732)
  print $ test (" \n(\t(\n86.63\t) *  \n\t-(\n64.2 )  +11.57\n\t*60.98\t /( 23.03\n) +\t(\n19.94\n*51.41 ) \n)+\t79.68 *-(\n\t20.74\t-\t \t59.4 * 47.64 \n+\n\t98.52 - -35.11 /( 96.87\t-\t5.72\t)\n\n/(\t19.48\n) - 30.65 + 57.27\n   ) \t\n/(\t(\n58.83\n/( (\t1.08\n))\n) )\t/( (\t79.22\t- \t58.61\t- \t50.65+\n61.78 \t *\n\n13.03* 36.55\n \n-\t 19.94\n+ \n 45.5+\t0.6 -  49.1 +\n\t71.05\n/(\n--82.75\t\t-76.04\n+ 23.1 * (\n68.57\t)\n- \t -(\t  (\n66.99 )\n/( \t59.32\t*--13.05\t*\t92.86 \n\n)\n-\n--43.8 \n * 13.93\t\n)\t\t-\t57.03+ \n(\t43.81 )\n+ 64.68\n\n/(\n85.94 )\t\t-\t\n4.36*\t 24.53/( 80.2\t) \t  )\t\n\n/(\n24.11\n) \t\n)\t)\n", -4505.761294455091)
