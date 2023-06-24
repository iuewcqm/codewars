-- https://www.codewars.com/kata/5443dd2d7fc4478154000ac6

module Fixit (foldr', reverse') where

import Prelude
import Data.List (List(..), (:))

reverse' :: forall a. (List a -> List a) -> List a -> List a
reverse' _ Nil    = Nil
reverse' f (x:xs) = f xs <> singleton x

foldr' :: forall a b. ((a -> b -> b) -> b -> List a -> b) -> (a -> b -> b) -> b -> List a -> b
foldr' _ _ b Nil    = b
foldr' f g b (x:xs) = g x (f g b xs) 
