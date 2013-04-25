{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, ExistentialQuantification #-}

#include "SDL.h"

module Graphics.UI.SDL.Audio.Open (AudioSpec(..), openAudio, pauseAudio) where

import Control.Monad
import Data.Word
import Data.Int
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Graphics.UI.SDL.Audio

class Sample f where
  format :: f -> AudioFormat

instance Sample Word8 where
	format _ = AudioU8

instance Sample Int8 where
	format _ = AudioS8

instance Sample Word16 where
	format _ = AudioU16Sys

instance Sample Int16 where
	format _ = AudioS16Sys

type BufferCallback u f = (Ptr u -> Ptr f -> CInt -> IO ())

foreign import ccall "wrapper"
  mkCallback :: BufferCallback u f -> IO (FunPtr (BufferCallback u f))

data AudioSpecInternal u f = AudioSpecInternal
  { freq' :: CInt
  , samples' :: Word16
  , callback' :: BufferCallback u f
  , channels' :: Word8
  , format' :: Word16
  , userdata' :: Ptr u
  }

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable (AudioSpecInternal u f) where
  alignment _ = #{alignment SDL_AudioSpec}
  sizeOf _ = #{size SDL_AudioSpec}
  poke ptr s = do
    #{poke SDL_AudioSpec, freq} ptr (freq' s)
    #{poke SDL_AudioSpec, samples} ptr (samples' s)
    cb <- mkCallback (callback' s)
    #{poke SDL_AudioSpec, callback} ptr cb
    #{poke SDL_AudioSpec, channels} ptr (channels' s)
    #{poke SDL_AudioSpec, format} ptr (format' s)
    #{poke SDL_AudioSpec, userdata} ptr (userdata' s)

data AudioSpec f = AudioSpec
  { freq :: Int
  , samples :: Word16
  , callback :: Ptr f -> Int -> IO ()
  , channels :: Word8
  }

specToInternal :: forall f u. Sample f => AudioSpec f -> AudioSpecInternal u f
specToInternal s
  = AudioSpecInternal
    { freq' = fromIntegral (freq s)
    , samples' = samples s
    , callback' = \u b l -> callback s b (fromIntegral l)
    , channels' = channels s
    , format' = fromAudioFormat $ format (undefined :: f)
    , userdata' = nullPtr
    }

foreign import ccall "SDL_OpenAudio"
  sdl_openAudio :: Ptr (AudioSpecInternal u f) -> Ptr (AudioSpecInternal u f) -> IO CInt

openAudio :: Sample f => AudioSpec f -> IO ()
openAudio s = with (specToInternal s) (\sp -> alloca (void . sdl_openAudio sp))

foreign import ccall "SDL_PauseAudio"
  sdl_pauseAudio :: CInt -> IO ()

pauseAudio :: Bool -> IO ()
pauseAudio p = sdl_pauseAudio (if p then 1 else 0)