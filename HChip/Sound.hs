module HChip.Sound where

import Data.Word
import Graphics.UI.SDL.Audio.Open

data Waveform = Triangle | Sawtooth | Pulse | Noise
  deriving (Show)

data Tone = Tone
	{	attack :: !Word8
	,	decay :: !Word8
	, sustain :: !Word8	
	, release :: !Word8	
	, volume :: !Word8	
	, wave :: !Waveform
	} deriving (Show)

data Sound = NoSound | Sound
	{ frequency :: !Word16
	, totalTime :: !Word16
	, elapsedTime :: !Word16
	, tone :: !Tone
	} deriving (Show)