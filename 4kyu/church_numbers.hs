-- https://www.codewars.com/kata/546dbd81018e956b51000077

{-# LANGUAGE RankNTypes #-}

module Haskell.Codewars.Church where

import Prelude hiding (succ)

newtype Number = Nr (forall a. (a -> a) -> a -> a)

zero :: Number
zero = Nr $ \_ z -> z

succ :: Number -> Number
succ (Nr a) = Nr $ \s z -> s (a s z)

one :: Number
one = succ zero

add :: Number -> Number -> Number
add (Nr a) (Nr b) = Nr $ \s z -> a s (b s z)

mult :: Number -> Number -> Number
mult (Nr a) (Nr b) = Nr $ \s z -> a (b s) z

pow :: Number -> Number -> Number
pow (Nr a) (Nr b) = Nr $ \s z -> (b a) s z

printNumber (Nr a) = print $ a (+1) 0

two = succ one
three = succ two

main = do
  printNumber zero
  printNumber one
  printNumber (add zero zero)
  printNumber (add one one)
  printNumber (add one (succ one))
  printNumber (mult one zero)
  printNumber (mult two two)
  printNumber (pow two three)
