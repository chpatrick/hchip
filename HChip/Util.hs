{-# LANGUAGE GADTs, FlexibleContexts #-}

module HChip.Util where

import Data.Array.MArray
import Data.Bits
import Data.Word

lowNibble :: (Num a, Bits a) => a -> a
lowNibble = (.&.)0xF

highNibble :: (Num a, Bits a) => a -> a
highNibble = (`shiftR`4)

buildInt :: (Num a, Bits a) => [ Word8 ] -> a
buildInt = foldr (\v a -> fromIntegral v .|. a `shiftL` 8) 0
