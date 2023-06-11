-- https://www.codewars.com/kata/54cf7f926b85dcc4e2000d9d

module Huffman
    ( frequencies
    , encode
    , decode
    , Bit (..)
    ) where

import Data.List
import Data.Ord (comparing)
import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)

data Bit = Z | O 
  deriving (Eq, Show)

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving Show

buildTree :: [(a, Int)] -> Tree a
buildTree freq = tree $ sortBy (comparing fst) $ [ (c, Leaf s) | (s, c)<- freq ]
  where 
    tree [(_, t)] = t
    tree ((c1,t1):(c2,t2):ts) = tree $ insertBy (comparing fst) (c1+c2, Branch t1 t2) ts

serialize :: Tree a -> [(a, [Bit])]
serialize (Leaf x)     = [(x, [])]
serialize (Branch l r) = [(x, Z:code) | (x, code) <- serialize l]
                      ++ [(x, O:code) | (x, code) <- serialize r]
                      
frequencies :: (Ord a, Eq a) => [a] -> [(a, Int)]
frequencies = map (head &&& length) . group . sort

encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode []   = const Nothing
encode [_]  = const Nothing
encode freq = Just <$> concat . mapMaybe (`lookup` huffman)
  where huffman = serialize $ buildTree freq

decode :: [(a, Int)] -> [Bit] -> Maybe [a]
decode []   = const Nothing
decode [_]  = const Nothing
decode freq = Just <$> decode' huffman
  where huffman = map swap . serialize $ buildTree freq
        decode' _ [] = []
        decode' ((h, c):hs) bs 
          | h `isPrefixOf` bs = c : decode' huffman (drop (length h) bs)
          | otherwise         = decode' hs bs
