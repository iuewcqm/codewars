-- https://www.codewars.com/kata/5922543bf9c15705d0000020

module ISO where

import Data.Either (isLeft)

import Data.Void
-- A type of `Void` have no value.
-- So it is impossible to construct `Void`,
-- unless using undefined, error, unsafeCoerce, infinite recursion, etc
-- And there is a function
-- absurd :: Void -> a
-- That get any value out of `Void`
-- We can do this becuase we can never have void in the zeroth place.

-- so, when are two type, `a` and `b`, considered equal?
-- a definition might be, it is possible to go from `a` to `b`,
-- and from `b` to `a`.
-- Going a roundway trip should leave you the same value.
-- Unfortunately it is virtually impossible to test this in Haskell.
-- This is called Isomorphism.

type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = ((bc . ab), (ba . cb))

-- We can combine isomorphism:
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = 
  (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (map ab, map ba)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba)  = ((<$>) ab, (<$>) ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (f, g)
  where f (Left  a) = Left  $ ab a
        f (Right c) = Right $ cd c
        g (Left  b) = Left  $ ba b
        g (Right d) = Right $ dc d
 
isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (\ac -> cd . ac . ba, \bd -> dc . bd . ab)

-- Going another way is hard (and is generally impossible)
-- Remember, for all valid ISO, converting and converting back
-- Is the same as the original value.
-- You need this to prove some case are impossible.
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (ab, ba) = (ab', ba')
  where ab' a = case ab (Just a) of
                  (Just a') -> a'
                  Nothing   -> case ab Nothing of
                                 (Just a'') -> a''
                                 Nothing    -> error "bad"
        ba' b = case ba (Just b) of
                  (Just b') -> b'
                  Nothing   -> case ba Nothing of
                                 (Just b'') -> b''
                                 Nothing    -> error "bad"

-- We cannot have
-- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
-- Note that we have
isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (ab, ba)
  where ab (Left a)   = Left (() : a)
        ab (Right ()) = Left []
        ba (Left [])  = Right ()
        ba (Left b)   = Left $ tail b
        ba (Right a)  = absurd a
        
-- where (), the empty tuple, has 1 value, and Void has 0 value
-- If we have isoUnEither,
-- We have ISO () Void by calling isoUnEither isoEU
-- That is impossible, since we can get a Void by substL on ISO () Void
-- So it is impossible to have isoUnEither

-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (symm, symm)


-- tests
shouldBe = (==)

bISO :: ISO Bool Bool
bISO = (not, not)

lrl :: ISO a b -> (a -> a)
lrl (ab, ba) = ba . ab

main = do
  print $ "substL"
  print $ (substL bISO True)     `shouldBe` False
  print $ (substL bISO False)    `shouldBe` True
  print $ (substL isoBool False) `shouldBe` False
  print $ (substL isoBool True)  `shouldBe` True

  print $ "substR"
  print $ (substR bISO True)     `shouldBe` False
  print $ (substR bISO False)    `shouldBe` True
  print $ (substR isoBool True)  `shouldBe` True
  print $ (substR isoBool False) `shouldBe` False

  print $ "isoEU"
  print $ (isLeft (substL isoEU (Right ()))) `shouldBe` True

  print $ "lrl isoEU (Left (replicate 5 ())) == Left (replicate 5 ())"
  print $ lrl isoEU (Left (replicate 5 ())) == Left (replicate 5 ()) 
