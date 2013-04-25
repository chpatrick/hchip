{-# LANGUAGE GADTs, FlexibleContexts #-}

module HChip.Util where

import Control.Exception
import Data.Array.IArray
import Data.Array.MArray
import Data.Bits
import Data.Int
import Data.Word
import System.Clock

{-# INLINE lowNibble #-}
lowNibble :: (Integral a, Num a, Bits a) => a -> Word8
lowNibble x = fromIntegral (x .&. 0xF)

{-# INLINE highNibble #-}
highNibble :: (Integral a, Num a, Bits a) => a -> Word8
highNibble = nibble 1

{-# INLINE nibble #-}
nibble :: (Integral a, Num a, Bits a) => Int -> a -> Word8
nibble n = lowNibble . (`shiftR`(n * 4))

buildInt :: (Num a, Bits a) => [ Word8 ] -> a
buildInt = foldr (\v a -> fromIntegral v .|. a `shiftL` 8) 0

(!?) :: (IArray a e, Ix i) => a i e -> i -> IO (Maybe e)
(!?) a i
  = catch (fmap Just $ evaluate (a ! i)) wrap
    where
      wrap :: SomeException -> IO (Maybe a)
      wrap e = return Nothing

sign :: Word16 -> Int16
sign = fromIntegral

unsign :: Int16 -> Word16
unsign = fromIntegral

byte :: (Integral a, Bits a) => Int -> a -> Word8
byte n x = fromIntegral ((x `shiftR` (n * 8)) .&. 0xFF)

time :: IO Word64
time = do
	TimeSpec s n <- getTime Monotonic
	return (fromIntegral s * 1000000000 + fromIntegral n)
