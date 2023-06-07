-- https://www.codewars.com/kata/5917f22dd2563a36a200009c

module ISO where

import Data.Void

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
isoMaybe (ab, ba)  = (fmap ab, fmap ba)

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

-- Sometimes, we can treat a Type as a Number:
-- if a Type t has n distinct value, it's Number is n.
-- This is formally called cardinality.
-- See https://en.wikipedia.org/wiki/Cardinality

-- Void has cardinality of 0 (we will abbreviate it Void is 0).
-- () is 1.
-- Bool is 2.
-- Maybe a is 1 + a.
-- We will be using peano arithmetic so we will write it as S a.
-- https://en.wikipedia.org/wiki/Peano_axioms
-- Either a b is a + b.
-- (a, b) is a * b.
-- a -> b is b ^ a. Try counting (() -> Bool) and (Bool -> ())

-- Algebraic data type got the name because
-- it satisfies a lot of algebraic rules under isomorphism

-- a = b -> c = d -> a * c = b * d
isoProd :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoProd = isoTuple 

-- a = b -> c = d -> a + c = b + d
isoPlus :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoPlus = isoEither

-- a = b -> S a = S b
isoS :: ISO a b -> ISO (Maybe a) (Maybe b)
isoS = isoMaybe

-- a = b -> c = d -> c ^ a = d ^ b
isoPow :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoPow = isoFunc

-- a + b = b + a
plusComm :: ISO (Either a b) (Either b a)
plusComm = (f, g)
  where f (Left  a) = Right a
        f (Right b) = Left b
        g (Left  b) = Right b
        g (Right a) = Left a
        
-- a + b + c = a + (b + c)
plusAssoc :: ISO (Either (Either a b) c) (Either a (Either b c))
plusAssoc = (f, g)
  where f (Left (Left  a))  = Left a
        f (Left (Right b))  = Right (Left  b)
        f (Right c)         = Right (Right c)
        g (Left a)          = Left (Left a)
        g (Right (Left  b)) = Left (Right b)
        g (Right (Right c)) = Right c

-- a * b = b * a
multComm :: ISO (a, b) (b, a)
multComm = (f, f)
  where f (a, b) = (b, a)

-- a * b * c = a * (b * c)
multAssoc :: ISO ((a, b), c) (a, (b, c))
multAssoc = (\((a, b), c) -> (a, (b, c)), \(a, (b, c)) -> ((a, b), c))

-- dist :: a * (b + c) = a * b + a * c
dist :: ISO (a, (Either b c)) (Either (a, b) (a, c))
dist = (f, g)
  where f (a, Left  b) = Left  (a, b)
        f (a, Right c) = Right (a, c)
        g (Left  (a, b)) = (a, Left  b)
        g (Right (a, c)) = (a, Right c)

-- (c ^ b) ^ a = c ^ (a * b)
curryISO :: ISO (a -> b -> c) ((a, b) -> c)
curryISO = (\f (a, b) -> f a b, \g a b -> g (a, b))

-- 1 = S O (we are using peano arithmetic)
-- https://en.wikipedia.org/wiki/Peano_axioms
one :: ISO () (Maybe Void)
one = (const Nothing, const ())

-- 2 = S (S O)
two :: ISO Bool (Maybe (Maybe Void))
two = (f, g)
  where f False = Nothing
        f True  = Just Nothing
        g (Just Nothing) = True
        g Nothing        = False

-- O + b = b
plusO :: ISO (Either Void b) b
plusO = (left, Right)
  where
    left (Left  x) = absurd x -- absurd :: Void -> a
    left (Right x) = x

-- S a + b = S (a + b)
plusS :: ISO (Either (Maybe a) b) (Maybe (Either a b))
plusS = (f, g)
  where f (Left Nothing)  = Nothing
        f (Left (Just a)) = Just $ Left a
        f (Right b)       = Just $ Right b
        g Nothing          = Left Nothing
        g (Just (Left  a)) = Left $ Just a
        g (Just (Right b)) = Right b

-- 1 + b = S b
plusSO :: ISO (Either () b) (Maybe b)
plusSO = isoPlus one refl `trans` plusS `trans` isoS plusO

-- O * a = O
multO :: ISO (Void, a) Void
multO = (fst, \v -> (v, absurd v))

-- S a * b = b + a * b
multS :: ISO (Maybe a, b) (Either b (a, b))
multS = (f, g)
  where f (Nothing, b) = Left b
        f (Just a,  b) = Right (a, b)
        g (Left b)       = (Nothing, b)
        g (Right (a, b)) = (Just a, b)

-- 1 * b = b
multSO :: ISO ((), b) b
multSO =
  isoProd one refl `trans`
    multS `trans`
    isoPlus refl multO `trans` 
    plusComm `trans`
    plusO

-- a ^ O = 1
powO :: ISO (Void -> a) ()
powO = (const (), \_ v -> absurd v)

-- a ^ (S b) = a * (a ^ b)
powS :: ISO (Maybe b -> a) (a, b -> a)
powS = (f, g)
  where f f' = (f' Nothing, f' . Just) 
        g (a, g') m = case m of
          Nothing -> a
          Just b  -> g' b
      
-- a ^ 1 = a
-- Go the hard way (like multSO, plusSO)
-- to prove that you really get what is going on!
powSO :: ISO (() -> a) a
powSO = (($ ()), const)


-- tests
shouldBe :: Eq a => a -> a -> Bool
shouldBe = (==)

lrl :: ISO a b -> (a -> a)
lrl (ab, ba) = ba . ab

rlr :: ISO a b -> (b -> b)
rlr (ab, ba) = ab . ba

str :: String
str = "what"

pa :: ISO
  (Either (Either Int Bool) String)
  (Either Int (Either Bool String))
pa = plusAssoc

main = do
  print $ lrl pa (Left  (Left     0)) `shouldBe` (Left  (Left     0))
  print $ lrl pa (Left  (Right True)) `shouldBe` (Left  (Right True))
  print $ lrl pa (Right          str) `shouldBe` (Right          str)
  print $ rlr pa (Left             0) `shouldBe` (Left             0)
  print $ rlr pa (Right (Left  True)) `shouldBe` (Right (Left  True))
  print $ rlr pa (Right (Right  str)) `shouldBe` (Right (Right  str))
