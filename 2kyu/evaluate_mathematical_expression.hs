-- https://www.codewars.com/kata/52a78825cdfc2cfc87000005

import Prelude hiding (div)

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

nat :: ReadP String
nat = munch1 isDigit

number :: ReadP Double
number = do int  <- nat
            frac <- char '.' *> nat
            return $ read (int ++ '.' : frac)
     +++ do int <- nat
            return $ read int

ws :: ReadP String
ws = munch (`elem` " \t\n")

add = op '+' (+)
sub = op '-' (-)
mul = op '*' (*)
div = op '/' (/)

op c f = ws *> char c *> ws *> return f
     
brackets = char '(' *> ws *> expression <* ws <* char ')'

negated = (char '-' *> return negate) <*> factor

factor = number +++ brackets +++ negated
term   = mul +++ div
expr   = add +++ sub

expression :: ReadP Double
expression = factor `chainl1` term `chainl1` expr

calc :: String -> Double
calc = fst . head . readP_to_S (ws *> expression <* ws <* eof)


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
