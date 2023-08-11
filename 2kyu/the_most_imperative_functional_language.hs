-- https://www.codewars.com/kata/5453af58e6c920858d000823

module Imperative (def, var, lit, while, (+=), (-=), (*=)) where

newtype M a b = M { runM :: [a] -> (b, [a]) }
type V a b = (Int, a -> b)

instance Functor (M a) where
  fmap f m = M $ \heap -> case runM m heap of
    (v, heap') -> (f v, heap')

instance Applicative (M a) where
  pure v  = M $ \heap -> (v, heap)
  f <*> m = M $ \heap -> case runM f heap of
    (f, heap') -> case runM m heap' of
      (v, heap'') -> (f v, heap'')

instance Monad (M a) where
  return  = pure
  m >>= f = M $ \heap -> case runM m heap of
    (v, heap') -> case runM (f v) heap' of
      (v', heap'') -> (v', heap'')

def :: M a (V a b) -> b
def m = case runM m [] of
  ((x, f), heap) -> f $ heap !! x

var :: a -> M a (V a a)
var a = M $ \heap -> ((length heap, id), heap ++ [a])

lit x = (0, const x)

while :: (V a b) -> (b -> Bool) -> M a () -> M a ()
while (x, g) f act = M $ \heap ->
  if f (g $ heap !! x)
     then case runM act heap of
       (_, heap') -> runM (while (x, g) f act) heap'
  else ((), heap)

update :: [a] -> Int -> a -> [a]
update [   ] _ _  = []
update (h:t) 0 h' = h' : t
update (h:t) n h' = h  : update t (n - 1) h'

(+=) :: (V Integer Integer) -> (V Integer Integer) -> M Integer ()
(a, fa) += (b, fb) = M $ \heap ->
  let va = fa $ heap !! a
      vb = fb $ heap !! b
  in ((), update heap a (va + vb))

(-=) :: (V Integer Integer) -> (V Integer Integer) -> M Integer ()
(a, fa) -= (b, fb) = M $ \heap ->
  let va = fa $ heap !! a
      vb = fb $ heap !! b
  in ((), update heap a (va - vb))

(*=) :: (V Integer Integer) -> (V Integer Integer) -> M Integer ()
(a, fa) *= (b, fb) = M $ \heap ->
  let va = fa $ heap !! a
      vb = fb $ heap !! b
  in ((), update heap a (va * vb))


-- tests
shouldBe :: Eq a => a -> a -> Bool
shouldBe = (==)

-- foldr (*) 1
factorial :: Integer -> Integer
factorial n = def $ do
  result <- var 1
  i      <- var n
  while i (>0) $ do
    result *= i
    i      -= lit 1
  return result
 
-- ((max 0 . subtract 1) .) . subtract
howManyBetween :: Integer -> Integer -> Integer
howManyBetween c n = def $ do
  result <- var 0
  i      <- var (c + 1)
  while i (<n) $ do
    result += lit 1
    i      += lit 1
  return result

main :: IO()
main = do
  print $ "factorial"
  print $ "should return the same as the functional one"
  print $ (\x -> factorial x `shouldBe` foldr (*) 1 [1..x]) 5

  print "howManyBetween"
  print "should return the same as the functional one"
  print $ (\from to ->
        howManyBetween from to `shouldBe` (max 0 $ to - from - 1 :: Integer)) 5 10
