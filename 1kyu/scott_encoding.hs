-- https://www.codewars.com/kata/59c132fb70a3b7efd3000024

{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a    = SMaybe  { runMaybe  :: forall b. b -> (a -> b) -> b }
newtype SList a     = SList   { runList   :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b   = SPair   { runPair   :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair (SPair p) = (p $ const, p $ const id)

fromPair :: (a,b) -> SPair a b
fromPair (a,b) = SPair $ (\p -> p a b)

fst :: SPair a b -> a
fst (SPair p) = p $ const

snd :: SPair a b -> b
snd (SPair p) = p $ const id

swap :: SPair a b -> SPair b a
swap p = SPair $ (\p -> p b a)
  where (a, b) = toPair p

curry :: (SPair a b -> c) -> (a -> b -> c)
curry f = (\a b -> f $ fromPair (a, b))

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f = (\p -> f (fst p) (snd p))

toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe m) = m Nothing Just

fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing  = SMaybe $ \b _ -> b
fromMaybe (Just a) = SMaybe $ \_ f -> f a

isJust :: SMaybe a -> Bool
isJust m = case toMaybe m of
      Nothing -> False
      Just _  -> True

isNothing :: SMaybe a -> Bool
isNothing = not . isJust

catMaybes :: SList (SMaybe a) -> SList a
catMaybes (SList f) = f (SList const) $
  \(SMaybe g) l -> (g id cons) (catMaybes l)

toEither :: SEither a b -> Either a b
toEither (SEither e) = e Left Right

fromEither :: Either a b -> SEither a b
fromEither (Left  a) = SEither $ \f _ -> f a
fromEither (Right b) = SEither $ \_ g -> g b
 
isLeft :: SEither a b -> Bool
isLeft e = case toEither e of
      Left  _ -> True
      Right _ -> False

isRight :: SEither a b -> Bool
isRight = not . isLeft

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition (SList f) = f (SPair $ 
  \g -> g (SList const) (SList const)) $
  \(SEither z) l -> let SPair p = partition l
  in p (\a b -> SPair $ 
  (\h -> h (z cons (const id) a) 
           (z (const id) cons b)))

toList :: SList a -> [a]
toList (SList l) = l [] (\a f -> a : toList f)

fromList :: [a] -> SList a
fromList []     = SList $ \b _ -> b
fromList (x:xs) = SList $ \_ f -> f x $ fromList xs

cons :: a -> SList a -> SList a
cons a l = SList $ \_ f -> f a l

concat :: SList a -> SList a -> SList a
concat (SList f) (SList g) = SList $ 
  \b f' -> f (g b f') (\a g' -> f' a (concat g' (SList g)))

null :: SList a -> Bool
null (SList f) = f True (\_ _ -> False)

length :: SList a -> Int
length (SList f) = f 0 (\_ l -> 1 + length l)

map :: (a -> b) -> SList a -> SList b
map f (SList f') = SList $ 
  \b g -> f' b (\a l -> g (f a) (map f l))

zip :: SList a -> SList b -> SList (SPair a b)
zip (SList f) (SList g) = f (SList const) $ 
  \a l1 -> g (SList const) (\b l2 -> cons (fromPair (a, b)) (zip l1 l2))

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f z (SList g) = g z (\a l -> foldl f (f z a) l)

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f z (SList g) = g z (\a l -> f a (foldr f z l))

take :: Int -> SList a -> SList a
take n (SList f)
  | n <= 0    = SList const
  | otherwise = f (SList const) (\a l -> cons a (take (n-1) l))


-- tests
shouldBe :: Eq a => a -> a -> Bool
shouldBe = (==)

nil :: SList a
nil = SList const

reduce :: Num a => a -> SList a -> a
reduce i sl = i + 10 * runList sl 0 reduce

main = do
  print "The Maybe type"
  print "can be cast to Prelude.Maybe"
  print $ toMaybe (SMaybe const) `shouldBe` (Nothing :: Maybe Int)
  print $ toMaybe (SMaybe $ \_ f -> f 4) `shouldBe` Just 4

  print "can be cast from Prelude.Maybe"
  print $ runMaybe (fromMaybe (Nothing)) 0 (+1) `shouldBe` 0
  print $ runMaybe (fromMaybe (Just 4)) 0 (+1) `shouldBe` 5
  
  print "The list type"
  print "can be cast to []"
  print $ toList (SList const) `shouldBe` ([] :: [Int])
  print $ toList (SList $ \_ f -> f 1 (SList $ \_ g -> g 2 (SList const))) `shouldBe` [1,2]
  
  print "can be cast from []"
  print $ runList (fromList []) 0 reduce `shouldBe` 0
  print $ runList (fromList [1,2]) 0 reduce `shouldBe` 21
  
  print "The Either type"
  print "can be cast to Prelude.Either"
  print $ toEither (SEither $ \f _ -> f 3) `shouldBe` (Left 3 :: Either Int String)
  print $ toEither (SEither $ \_ f -> f "hello") `shouldBe` (Right "hello" :: Either Int String)
    
  print "can be cast from Prelude.Either"
  print $ runEither (fromEither (Left 3)) show id `shouldBe` "3"
  print $ runEither (fromEither (Right "hello" :: Either Int String)) show id `shouldBe` "hello"
  
  print "The pair type"
  print "can be cast to (,)"
  print $ toPair (SPair $ \f -> f 2 "hi") `shouldBe` (2, "hi")
    
  print "can be cast from (,)"
  print $ runPair (fromPair (2, "hi")) replicate `shouldBe` ["hi", "hi"]

  print "can evaluate length"
  print $ length (cons 1 $ cons 2 $ cons 3 $ cons 4 nil) `shouldBe` 4

  print "concat function"
  print $ toList (concat (cons 1 $ cons 2 nil) (cons 3 $ cons 4 nil))
  print $ toList (concat (cons 1 $ cons 2 nil) (cons 3 $ cons 4 nil)) `shouldBe` [1,2,3,4]
  
  print "map function"
  print $ toList (map (+3) (cons 1 $ cons 2 nil))
  print $ toList (map (+3) (cons 1 $ cons 2 nil)) `shouldBe` [4,5]

  print "take function"
  print $ toList (take 2 (cons 1 $ cons 2 $ cons 3 $ cons 4 nil)) `shouldBe` [1,2]

  print "foldr function"
  print $ foldr (+) 0 (cons 1 $ cons 10 $ cons 20 nil) `shouldBe` 31
  print $ (toList $ foldr cons nil (cons 1 $ cons 2 $ cons 3 nil)) `shouldBe` [1,2,3]
  
  print "foldl function"
  print $ foldl (+) 0 (cons 1 $ cons 10 $ cons 20 nil) `shouldBe` 31

  print "zip function"
  print $ (toPair <$> (toList $ zip (cons 1 $ cons 2 nil) (cons 3 $ cons 4 nil))) `shouldBe` [(1,3),(2,4)]
