{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module HChip.Machine where

import Control.Applicative
import Control.Lens
import Control.Monad.Random
import Control.Monad.State
import Data.Array.IO
import Data.Bits
import Data.Bits.Lens
import Data.Word
import System.Exit

import HChip.Util

newtype Emu a = Emu { runEmu :: RandT StdGen (StateT EmuState IO) a }
  deriving (Monad, MonadState EmuState, MonadIO, MonadRandom, Functor, Applicative)

data EmuState = EmuState
  { _pc :: Word16
  , _sp :: Word16
  , _flags :: Word8
  , _spriteFlip :: ( Bool, Bool )
  , _bgc   :: Word8
  , _spriteSize :: ( Word8, Word8 )
  , _vblank :: Bool
  , regs :: IOUArray Word8 Word16
  , memory :: IOUArray Word16 Word8
  }

makeLenses ''EmuState

carry :: Simple Lens EmuState Bool
carry = flags . bitAt 1

zero :: Simple Lens EmuState Bool
zero = flags . bitAt 2

overflow :: Simple Lens EmuState Bool
overflow = flags . bitAt 6

negative :: Simple Lens EmuState Bool
negative = flags . bitAt 7


class Loadable8 a where
  load8 :: a -> Emu Word8

class Savable8 a where
  save8 :: a -> Word8 -> Emu ()

class Loadable16 a where
  load16 :: a -> Emu Word16

class Savable16 a where
  save16 :: a -> Word16 -> Emu ()

newtype Register = Reg Word8
newtype Memory = Mem Word16
data PC = PC
data SP = SP
data Flags = Flags

instance Loadable16 Register where
  load16 (Reg n) = do
    rs <- gets regs
    liftIO $ readArray rs n

instance Savable16 Register where
  save16 (Reg n) v = do
    rs <- gets regs
    liftIO $ writeArray rs n v

instance Loadable16 PC where
  load16 PC = use pc

instance Savable16 PC where
  save16 PC = assign pc

instance Loadable16 SP where
  load16 SP = use sp

instance Savable16 SP where
  save16 SP = assign pc

instance Loadable8 Flags where
  load8 Flags = use flags

instance Savable8 Flags where
  save8 Flags = assign flags

instance Loadable8 Memory where
  load8 (Mem a) = do
    m <- gets memory
    liftIO $ readArray m a

instance Savable8 Memory where
  save8 (Mem a) v = do
    m <- gets memory
    liftIO $ writeArray m a v

instance Loadable16 Memory where
  load16 (Mem a) = do
    l <- load8 $ Mem a
    h <- load8 $ Mem (a + 1)
    return (buildInt [ l, h ])

instance Savable16 Memory where
  save16 (Mem a) v = do
    save8 (Mem a) (fromIntegral (v .&. 0xFF) :: Word8)
    save8 (Mem (a + 1)) (fromIntegral (v `shiftR` 8) :: Word8)

debug :: String -> Emu ()
debug = liftIO . putStrLn
