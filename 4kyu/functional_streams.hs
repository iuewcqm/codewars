-- https://www.codewars.com/kata/5512ec4bbe2074421d00028c

module Stream where

import Control.Arrow
import Control.Applicative

data Stream a = a :> Stream a
infixr :>

-- | Get the first element of a stream.
headS :: Stream a -> a
headS (a :> _) = a

-- | Drop the first element of a stream.
tailS :: Stream a -> Stream a
tailS (_ :> s) = s


-- {{{ Stream constructors

-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS = (:>) <*> (repeatS)

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :> iterateS f (f x)

-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS xs = foldr (:>) (cycleS xs) xs


-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS a = iterateS (+1) a


-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = iterateS (+s) x

-- }}}


-- | Fold a stream from the left.
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = f x (foldrS f xs)

-- | Filter a stream with a predicate.
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (x :> xs) = case p x of
          True  -> x :> filterS p xs
          False -> filterS p xs

-- | Take a given amount of elements from a stream.
takeS :: Int -> Stream a -> [a]
takeS i (a :> t) 
  | i <= 0    = []
  | otherwise = a : takeS (pred i) t

-- | Drop a given amount of elements from a stream.
dropS :: Int -> Stream a -> Stream a
dropS i s@(a :> t)
  | i <= 0    = s
  | otherwise = dropS (pred i) t
  
-- | Do take and drop simultaneous.
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS i s = (takeS i s, dropS i s)

-- | Combine two streams with a function.
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (x:>xs) (y:>ys) = f x y :> zipWithS f xs ys

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)

instance Functor Stream where
    -- fmap :: (a -> b) -> Stream a -> Stream b
    fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
    -- pure :: a -> Stream a
    pure = repeatS

    -- (<*>) :: Stream (a -> b) -> Stream a -> Stream b
    (f:>fs) <*> (x:>xs) = f x :> (fs <*> xs)

-- | The stream of fibonacci numbers.
fibS :: Stream Integer
fibS = 0 :> 1 :> zipWithS (+) fibS (tailS fibS)

-- | The stream of prime numbers.
primeS :: Stream Integer
primeS =
  let sieve (p:>xs) = p :> (sieve $ filterS ((>0) . (`mod` p)) xs)
  in sieve $ fromS 2


-- tests
shouldBe :: Eq a => a -> a -> Bool
shouldBe = (==)
    
afterHeadS = headS . tailS

main = do
  print "basics"
  print "headS on Int" 
  print $ headS (42 :> undefined) `shouldBe` 42
  print "tail" 
  print $ afterHeadS ('a' :> 'b' :> undefined) `shouldBe` 'b'
    
  print "peeking into streams"
  print "takeS should work for negative indices" 
  print $ take 2 (takeS (-1) $ 1 :> 2 :> undefined) `shouldBe` []

  print "stream constructors"
  print "repeatS" 
  print $ headS (repeatS 42) `shouldBe` 42
  print "iterateS" 
  print $ afterHeadS (iterateS (+ 1) 0) `shouldBe` 1
  print "cycleS" 
  print $ afterHeadS (cycleS [1 .. 9]) `shouldBe` 2
  print "fromS" 
  print $ afterHeadS (fromS 42) `shouldBe` 43
  print "fromStepS" 
  print $ afterHeadS (fromStepS 42 2) `shouldBe` 44

  print "general purpose functions"
  print "foldrS" 
  print $ all (== 42) (take 10 $ foldrS (:) (repeatS 42)) `shouldBe` True
  print "filterS" 
  print $ takeS 4 (filterS even $ fromS 0) `shouldBe` [0, 2, 4, 6]
  print "takeS" 
  print $ length (takeS 5 $ repeatS 42) `shouldBe` 5
  print "dropS" 
  print $ headS (dropS 10 $ fromS 0) `shouldBe` 10
  print "splitAtS" 
  print $ case splitAtS 1 (fromS 0) of { (ls, rs) -> (ls, headS rs) `shouldBe` ([0], 1) }
  print "zipWithS" 
  print $ headS (zipWithS (+) (repeatS 20) (repeatS 22)) `shouldBe` 42

  print "class instances"
  print "fmap" 
  print $ headS (fmap (+ 1) $ repeatS 1) `shouldBe` 2
  print "pure" 
  print $ takeS 2 (pure 42) `shouldBe` [42, 42]
  print "(<*>)" 
  print $ headS (pure (* 2) <*> pure 21) `shouldBe` 42

  print "sequences"
  print "fibonacci sequence" 
  print $ takeS 4 fibS `shouldBe` [0, 1, 1, 2]
  print "prime sequence" 
  print $ takeS 10 primeS `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
