-- https://www.codewars.com/kata/5c2fcbcba305ad2c4a91122d

{-# LANGUAGE TypeOperators, TypeFamilies, GADTs #-}

module Kata.AdditionAssoc where

data Z
data S n

data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

data Equal :: * -> * -> * where
  EqlZ :: Equal Z Z
  EqlS :: Equal n m -> Equal (S n) (S m)

type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ     = EqlZ
reflexive (NumS n) = EqlS $ reflexive n

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ     = EqlZ
symmetric (EqlS n) = EqlS $ symmetric n

-- This is the proof that the kata requires.
-- | a + (b + c) = (a + b) + c
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc NumZ NumZ NumZ = EqlZ
plusAssoc NumZ NumZ (NumS c) = EqlS $ plusAssoc NumZ NumZ c
plusAssoc NumZ (NumS b) c = EqlS $ plusAssoc NumZ b c
plusAssoc (NumS a) b c = EqlS $ plusAssoc a b c
