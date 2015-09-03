-- |
-- Module      : Data.Encode.Huffman.Class
-- Copyright   : Shingo OKAWA, 2015
-- License     : BSD3
-- Maintainer  : shingo.okawa.n.a@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Internal Datatype definitions for Huffman encoding.
module Data.Encode.Huffman.Tree
    ( Node (..)
    , encode
    ) where

import Data.HashMap.Strict (HashMap, empty, insert, toList, fromlist)
import Data.Maybe          (fromJust)
import Data.Encode.Huffman.Class
import qualified Data.Heap as H

-- | A class of codes which is generated from a given context, i.e.,
--   inputed String.
type Dictionary = Dict {
      dictionary :: HashMap String Context,
      size       :: Int,
      length     :: Int
    } deriving (Eq)

-- | A datatype of internal heap trees for Huffman encoding.
data Node = Leaf String Context
          | Fork Node Node Context
            deriving (Show)

-- | Encodes the given word-frequency map into the contextized word map.
encode :: HashMap String Int -> HashMap String Context
encode = generate . aggregate . heapify

-- Makes a heap tree from the given hash map.
heapify :: HashMap String Int -> H.MinHeap Node
heapify = foldl (flip H.insert) H.empty . fst . foldl contextize ([], 0) toList
    where contextize :: ([Node], Int) -> (String, Int) -> ([Node], Int)
          contextize acc@(ns, i) (w, f) = ((Leaf w (Cxt i f [])):ns, i + 1, [])

-- Aggregates words within the specified heap tree.
aggregate :: H.MinHeap Node -> H.MinHeap Node
aggregate h = foldl aggregate' h [0..until]
    where until          = H.size h
          aggregate' h i = let (n', h') = fromJust H.view h
                               frequency :: Node -> Int
                               frequency (Leaf _   cxt@(_ f _ _)) = f
                               frequency (Fork _ _ cxt@(_ f _ _)) = f
                           in  case H.view h' of
                                 Just (n'', h'') -> let f    = frequency n' + frequency n''
                                                        cxt  = Cxt i f [] []
                                                        n''' = Fork n' n'' cxt
                                                    in  H.insert n''' h''
                                 Nothing         -> h

-- Generates Huffman code from the given heapified context.
generate :: H.MinHeap Node -> HashMap String Context
generate h = generate' (fst $ fromJust $ H.view h) [] [] empty
    where generate' (Leaf w   cxt) c occs acc = insert w cxt {code = c, occurences = occs} acc
          generate' (Fork l r cxt) c occs acc = let occs' = index cxt : occs
                                                    acc'  = generate' l (Zero : c) occs' acc
                                                in  generate' r (One : c) occs' acc'
