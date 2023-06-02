-- https://www.codewars.com/kata/547202bdf7587835d9000c46/train/haskell

{-# LANGUAGE NoImplicitPrelude #-}
module Monads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return x = Identity x
  (Identity v) >>= f = f v

instance Monad Maybe where
  return x = Just x
  Nothing  >>= f = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return a = State $ \input -> (a, input)
  (State g) >>= f = State $ \input -> let
    (a, input') = g input
    in runState (f a) input'

instance Monad (Reader s) where
  return a = Reader $ \input -> a
  (Reader g) >>= f = Reader $ \input -> runReader (f $ g input) input
  
instance Monoid w => Monad (Writer w) where
  return a = Writer (mempty, a)
  (Writer (s, v)) >>= f = let 
    (input, a) = runWriter (f $ v)
    in Writer (input `mappend` s, a)
