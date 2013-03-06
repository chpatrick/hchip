{-# LANGUAGE GADTs, FlexibleContexts #-}

module HChip.Util where

import Control.Exception
import Control.Lens
import Data.Array.IArray
import Data.Array.MArray
import Data.Bits
import Data.Word

lowNibble :: (Num a, Bits a) => a -> a
lowNibble = (.&.)0xF

highNibble :: (Num a, Bits a) => a -> a
highNibble = (`shiftR`4)

buildInt :: (Num a, Bits a) => [ Word8 ] -> a
buildInt = foldr (\v a -> fromIntegral v .|. a `shiftL` 8) 0

(!?) :: (IArray a e, Ix i) => a i e -> i -> IO (Maybe e)
(!?) a i
  = catch (fmap Just $ evaluate (a ! i)) wrap
    where
      wrap :: SomeException -> IO (Maybe a)
      wrap e = return Nothing
