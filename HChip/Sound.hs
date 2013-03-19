module HChip.Sound where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans
import Data.Array
import Data.Bits
import Data.IORef
import Data.Int
import Data.Fixed
import Data.Word
import Foreign.Ptr
import Foreign.Marshal.Array
import Graphics.UI.SDL.Audio.Open
import System.Random

import HChip.Machine
import HChip.Util

audioSamples :: Word64
audioSamples = 16384

sampleRate :: Word64
sampleRate = 22050

wavefunc :: Tone -> Double -> IO (Word64 -> IO Double)
wavefunc Simple f = return (\t -> return (triangle f t))
wavefunc ADSR { wave = Sawtooth } f = return (\t -> return (sawtooth f t))
wavefunc ADSR { wave = Triangle } f = return (\t -> return (triangle f t))
wavefunc ADSR { wave = Pulse    } f = return (\t -> return (pulse f t))
wavefunc ADSR { wave = Noise    } f = do
  sr <- newIORef 0
  return (noise sr (round (wavelength f)))

attacks :: Array Word8 Word64
attacks = listArray (0x0, 0xF) [ 2, 8, 16, 24, 38, 56, 68, 80, 100, 250, 500, 800, 1000, 3000, 5000, 8000 ]

decays :: Array Word8 Word64
decays = listArray (0x0, 0xF) [ 6, 24, 48, 72, 114, 168, 204, 240, 300, 750, 1500, 2400, 3000, 9000, 15000, 24000 ]

releases :: Array Word8 Word64
releases = listArray (0x0, 0xF) [ 6, 24, 48, 72, 114, 168, 204, 240, 300, 750, 1500, 2400, 3000, 9000, 15000, 24000 ]

stepSlope :: SoundStep -> Double
stepSlope s = (stepEnd s - stepStart s) / fromIntegral (stepTime s)

step :: SoundStep -> Word64 -> Double
step s t = stepStart s + fromIntegral t * stepSlope s

timeToSamples :: Word64 -> Word64
timeToSamples t = t * sampleRate `div` 1000

genPlan :: Word64 -> Tone -> SoundPlan
genPlan tm Simple = [ SoundStep (timeToSamples tm) 1 1 ]
genPlan tm ADSR { attack = a, decay = d, sustain = s, release = r, volume = v } = fst $ cutPlan tt
  [ SoundStep at 0 v
  , SoundStep dt v s
  , SoundStep st s s
  , SoundStep rt s 0
  ] where
    tt = timeToSamples tm
    at = timeToSamples (attacks ! a)
    dt = timeToSamples (decays ! d)
    st = tt - min (at + dt + rt) tt
    rt = timeToSamples (releases ! r)

cutPlan :: Word64 -> SoundPlan -> ( SoundPlan, SoundPlan )
cutPlan t [] = ( [], [] )
cutPlan t (s : ss)
  | t <= stepTime s = ( [ SoundStep t (stepStart s) cv ], SoundStep (stepTime s - t) cv (stepEnd s) : ss )
  | otherwise = let ( ss', ss'') = cutPlan (t - stepTime s) ss in ( s : ss', ss'' )
    where
      cv = step s t

planLength = sum . map stepTime

planVolumes = concatMap (\s -> map (step s) [1..stepTime s])

initSound :: IO (MVar (Maybe Sound))
initSound = do
  sd <- newMVar Nothing
  openAudio AudioSpec { freq = fromIntegral sampleRate, samples = fromIntegral audioSamples, channels = 1, callback = fillBuf sd }
  return sd

wavelength :: Double -> Double
wavelength f = fromIntegral sampleRate / f

-- WAVEFORMS

sawtooth :: Double -> Word64 -> Double
sawtooth f t = (fromIntegral t * 256 / wavelength f) `mod'` 256 - 128

triangle :: Double -> Word64 -> Double
triangle f t = 128 - abs ((fromIntegral t * 512 / wavelength f) `mod'` 512 - 256)

pulse :: Double -> Word64 -> Double
pulse f t = signum (fromIntegral t `mod'` wavelength f - wavelength f / 2) * 127

noise :: IORef Double -> Word64 -> Word64 -> IO Double
noise sr wl t
  | t `mod ` wl == 0 = do
    s <- getStdRandom (randomR (-128, 127))
    writeIORef sr s
    return s
  | otherwise = readIORef sr

fillBuf :: MVar (Maybe Sound) -> Ptr Word8 -> Int -> IO ()
fillBuf sd b l = do
  s <- takeMVar sd
  case s of
    Nothing -> pokeArray b (replicate l 0) >> putMVar sd Nothing
    Just s -> do
      let ( sp', sp'' ) = cutPlan (fromIntegral l) (soundPlan s)
      let es = elapsedSamples s
      let sc = planLength sp'
      if null sp''
        then pauseAudio True >> putMVar sd Nothing
        else putMVar sd $! Just (s { elapsedSamples = es + sc, soundPlan = sp'' })
      let vs = planVolumes sp'
      ws <- mapM (waveform s) [es..es + sc - 1]
      let ss = zipWith (\v w -> round (v * w)) vs ws ++ replicate (l - fromIntegral sc) 0
      pokeArray b ss

killSound :: Emu ()
killSound = do
  sd <- gets sound
  liftIO $ do 
    takeMVar sd
    pauseAudio True -- we should be in control of sound when pausing
    putMVar sd Nothing

play :: Word16 -> Word16 -> Emu ()
play f t = unless (f == 0 || t == 0) $ do
  sd <- gets sound
  tn <- use tone
  let sp = genPlan (fromIntegral t) tn
  liftIO $ do
    w <- wavefunc tn (fromIntegral f)
    takeMVar sd
    pauseAudio False
    putMVar sd $ Just Sound {elapsedSamples = 0, soundPlan = sp, waveform = w }

sng :: Word8 -> Word16 -> Emu ()
sng ad vtsr = do
  let w = fromIntegral (nibble 2 vtsr)
  when (0x00 <= w && w <= 0x04) $ tone .= ADSR {
        attack = highNibble ad
      , decay = lowNibble ad
      , sustain = fromIntegral (nibble 3 vtsr) / 0xF
      , release = (nibble 2 vtsr)
      , volume = fromIntegral (lowNibble vtsr) / 0xF
      , wave = toEnum w
      }  
