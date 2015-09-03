{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
-- |
-- Module      : Data.Encode.Huffman.Class
-- Copyright   : Shingo OKAWA, 2015
-- License     : BSD3
-- Maintainer  : shingo.okawa.n.a@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Typeclass and Datatype definitions for Huffman encoding.
module Data.Encode.Huffman.Class
    ( Bit     (..)
    , Code    (..)
    , Context (..)
    ) where

-- | A data represents bitwise objects.
data Bit = Zero
         | One
           deriving (Eq, Ord, Show, Read)

-- | Synonym for binary representations.
type Code = [Bit]

instance Enum Code where
    toEnum n =
        case n `mod` 2 of
          0 -> Zero : toEnum' (n `div` 2)
          1 -> One  : toEnum' (n `div` 2)
        where toEnum' 0 = []
              toEnum' n = toEnum n
    fromEnum []       = 0
    fromEnum (Zero:b) = 2 * fromEnum b
    fromEnum (One:b)  = 2 * fromEnum b + 1

-- | Represents Huffman encoding context.
data Context = Cxt {
      -- Index of word within corpus.
      index      :: Int,
      -- Frequency word within corpus.
      frequency  :: Int,
      -- Huffman code of a word.
      code       :: Code,
      -- Occurences of the word.
      occurences :: [Int]
    } deriving (Eq, Show, Read)
