{-# LANGUAGE OverloadedStrings #-}

module HChip.Loader where

import HChip.Util

import Control.Exception
import Control.Monad
import Control.Monad.Error
import Data.Binary.Get
import Data.ByteString
import qualified Data.ByteString.Lazy as BSL
import Data.Bits
import Data.Digest.CRC32
import Data.Word

data Assembly = Assembly
  { version :: (Word8, Word8)
  , pc :: Word16
  , rom :: ByteString 
  } deriving (Show)

parseAssembly :: Get Assembly
parseAssembly = do
  magic <- getBytes 4
  unless (magic == "CH16") $ fail "Invalid magic string."
  skip 4
  versionByte <- getWord8
  romSize <- getWord32le
  pc <- getWord16le
  crc <- getWord32le
  rom <- getBytes (fromIntegral romSize)
  unless (crc32 rom == crc) $ fail "Invalid checksum."
  return $ Assembly (highNibble versionByte, lowNibble versionByte) pc rom

-- ugly wrapper due to Get using fail
runGet' :: Get a -> BSL.ByteString -> IO (Either String a)
runGet' p s
  = catch (fmap Right $ evaluate $ runGet p s) wrap
    where
      wrap :: SomeException -> IO (Either String a)
      wrap = return . Left . show

