-- |
-- Module      : Data.Encode.Huffman
-- Copyright   : Shingo OKAWA, 2015
-- License     : BSD3
-- Maintainer  : shingo.okawa.n.a@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- APIs for Huffman encoding.
module Data.Encode.Huffman.Class
    ( Dictionary
--    , load
--    , encode
--    , decode
--    , index
--    , occurrences
--    , frequency
    ) where

import Control.Monad             (foldM)
import Data.Char                 (toLower)
import Data.HashMap.Strict       (HashMap, elems, insertWith, toList)
import Data.List                 (intersperse)
import NLP.Tokenize              (tokenize)
import Data.Encode.Huffman.Class
import Data.Encode.Huffman.Tree
import qualified NLP.Tokenize as NT

-- | A class of codes which is generated from a given context, i.e.,
--   inputed String.
type Dictionary = Dict {
      -- Holds rendered contexts for each appered words, i.e., encoded codes,
      -- apperence poitions and so on.
      contexts :: HashMap String Context,
      -- Holds size of the dictionary.
      size     :: Int,
      -- Holds the code length.
      length   :: Int
    } deriving (Eq)

instance Show Dictionary where
    show (Dict cxts sz len) = concat $ intersperse "," [show $ toList cxts, show size, show len]

-- Tokenizes the given String into words.
tokenize :: String -> [String]
tokenize = map (map toLower) . filter (~= "^[a-zA-Z-]+$") . NT.tokenize

-- Counts up words within the assigned String.
count:: HashMap String Int -> String -> HashMap String Int
count acc = foldl count' acc . tokenize

-- Take the assigned word into the accont of counting word context.
count' :: HashMap String Int -> String -> HashMap String Int
count' acc w = insertWith (+) w 1 acc

-- Converts the aggregated word-frequency map into dictionary.
dictionarize :: HashMap String Int -> Dictionary
dictionarize hist = let cxts = contextize $ hist
                        len  = maximum (map (length . code) $ elems cxt)
                    in  Dict cxts (size cxts) len

