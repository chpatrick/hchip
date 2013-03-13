module HChip.Sound where

import Control.Concurrent.MVar
import Control.Monad.State
import Control.Monad.Trans
import Data.Bits
import Data.IORef
import Data.Int
import Data.Ratio
import Data.Word
import Foreign.Ptr
import Foreign.Marshal.Array
import Graphics.UI.SDL.Audio.Open

import HChip.Machine

audioSamples = 8192
sampleRate = 22050
--secPerSample = (1 % sampleRate) :: Ratio Int

initSound :: IO (MVar Sound)
initSound = do
	sd <- newMVar NoSound
	openAudio AudioSpec { freq = sampleRate, samples = audioSamples, channels = 1, callback = fillBuf sd }
	return sd

sawtooth :: Int -> Int -> Int8
sawtooth f t = fromIntegral (t * 256 * f `div` sampleRate)

fillBuf :: MVar Sound -> Ptr Int8 -> Int -> IO ()
fillBuf sd b l = modifyMVar_ sd fillBuf'
  where
  	fillBuf' NoSound = pokeArray b (replicate l 0) >> return NoSound
  	fillBuf' s@Sound { frequency = f, totalSamples = ts, elapsedSamples = es } = do
  		let sc = min (ts - es) l
  		let ss = map (sawtooth (fromIntegral f)) [es..es + sc - 1] ++ replicate (l - sc) 0
  		pokeArray b ss
  		let es' = es + sc
  		return $ if es' < ts
  			then s { elapsedSamples = es' }
  		  else NoSound

killSound :: Emu ()
killSound = do
--	liftIO $ pauseAudio True
	sd <- gets sound
	liftIO $ putMVar sd NoSound

play :: Word16 -> Word16 -> Emu ()
play f t = do
	sd <- gets sound
	let tn = Tone 0 0 0 0 0 Sawtooth
	liftIO $ swapMVar sd Sound { frequency = fromIntegral f, totalSamples = fromIntegral t * sampleRate `div` 1000, elapsedSamples = 0, tone = tn }
	liftIO $ pauseAudio False
