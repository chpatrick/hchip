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

import HChip.Machine
import HChip.Util

audioSamples :: Word64
audioSamples = 8192

sampleRate :: Word64
sampleRate = 22050
--secPerSample = (1 % sampleRate) :: Ratio Int

-- TODO: other waveforms
wavefunc :: Tone -> Double -> Word64 -> Double
wavefunc Simple = sawtooth
wavefunc ADSR { wave = Sawtooth } = sawtooth
wavefunc ADSR { wave = Triangle } = sawtooth
wavefunc ADSR { wave = Noise    } = sawtooth
wavefunc ADSR { wave = Pulse    } = sawtooth

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
genPlan tm ADSR { attack = a, decay = d, sustain = s, release = r, volume = v } = 
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

planVolumes = concatMap (\s -> map (step s) [1..stepTime s])

initSound :: IO (MVar (Maybe Sound))
initSound = do
  sd <- newMVar Nothing
  openAudio AudioSpec { freq = fromIntegral sampleRate, samples = fromIntegral audioSamples, channels = 1, callback = fillBuf sd }
  return sd

wavelength :: Double -> Double
wavelength f = fromIntegral sampleRate / f

sawtooth :: Double -> Word64 -> Double
sawtooth f t = (fromIntegral t * 256 / wavelength f) `mod'` 256 - 128

fillBuf :: MVar (Maybe Sound) -> Ptr Word8 -> Int -> IO ()
fillBuf sd b l = modifyMVar_ sd fillBuf'
  where
    fillBuf' Nothing = pokeArray b (replicate l 0) >> return Nothing
    fillBuf' (Just s) = do
      let ( sp', sp'' ) = cutPlan (fromIntegral l) (soundPlan s)
      let vs = planVolumes sp'
      let es = elapsedSamples s
      let sc = fromIntegral $ length vs
      let ws = map (waveform s) [es..es + sc - 1]
      let ss = zipWith (\v w -> round (v * w)) vs ws ++ replicate (l - fromIntegral sc) 0
      pokeArray b ss
      if null sp''
        then pauseAudio True >> return Nothing
        else return $! Just (s { elapsedSamples = es + sc, soundPlan = sp'' })

killSound :: Emu ()
killSound = do
--  liftIO $ pauseAudio True
  sd <- gets sound
  liftIO $ putMVar sd Nothing

play :: Word16 -> Word16 -> Emu ()
play f t = do
  sd <- gets sound
  tn <- use tone
  let sp = genPlan (fromIntegral t) tn
  liftIO $ print t
  liftIO $ print sp
  liftIO $ swapMVar sd $ Just Sound {elapsedSamples = 0, soundPlan = sp, waveform = wavefunc tn (fromIntegral f) }
  liftIO $ pauseAudio False

sng :: Word8 -> Word16 -> Emu ()
sng ad vtsr = do
  let t = ADSR {
      attack = highNibble ad
    , decay = lowNibble vtsr
    , volume = fromIntegral (nibble 3 vtsr) / 16
    , wave = toEnum $ fromIntegral $ nibble 2 vtsr
    , sustain = fromIntegral (nibble 1 vtsr) / 16
    , release = lowNibble vtsr
    }  
  liftIO $ print t
  tone .= t