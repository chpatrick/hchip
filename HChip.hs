{-# LANGUAGE StandaloneDeriving #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Array.IO
import Data.Word
import Graphics.UI.SDL as SDL
import Text.Printf
import System.Clock
import System.IO
import System.Environment
import System.Random

import HChip.CPU
import HChip.Debug
import HChip.Loader
import HChip.Machine
import HChip.Ops
import HChip.Sound
import HChip.Util

main = do
  fs <- getArgs
  case fs of
    [ f ] -> main' f
    _ -> putStrLn "Usage: hchip <rom>"

main' f = do
  rom <- BSL.readFile f
  case loadAssembly rom of
    Left e -> putStrLn ("Problem parsing ROM: " ++ e)
    Right a -> do 
      ( fb, bb ) <- initSDL
      s <- initState a fb bb
      rng <- getStdGen
      evalStateT (evalRandT (runEmu mainLoop) rng) s
      liftIO $ putStrLn ""
      quit

initSDL :: IO ( Surface, Surface )
initSDL = do 
  SDL.init [ InitVideo, InitAudio ]
  fb <- setVideoMode 320 240 32 [ HWSurface ]
  bb <- createRGBSurface [ SWSurface ] 320 240 8 0xFF 0xFF 0xFF 0xFF
  setColors bb defaultPalette 0
  return ( fb, bb )

setPad :: Bool -> Keysym -> Emu ()
setPad s (Keysym k _ _) = let l = case k of {
  ; SDLK_UP -> Just padUp
  ; SDLK_RIGHT -> Just padRight
  ; SDLK_DOWN -> Just padDown
  ; SDLK_LEFT -> Just padLeft
  ; SDLK_q -> Just padStart
  ; SDLK_w -> Just padSelect
  ; SDLK_x -> Just padB
  ; SDLK_z -> Just padA
  ; _ -> Nothing
  }
  in case l of
    Nothing -> return ()
    Just l -> do
      m <- gets memory
      p <- liftIO $ readArray m 0xFFF0
      liftIO $ writeArray m 0xFFF0 (p & l .~ s)

processEvents :: Emu Bool
processEvents = do
  e <- liftIO pollEvent
  ( q, l ) <- case e of
    Quit -> return ( True, False )
    NoEvent -> return ( False, False )
    KeyDown k -> setPad True k >> return ( False, True )
    KeyUp k -> setPad False k >> return ( False, True )
    _ -> return ( False, True )
  if l then processEvents else return q

mainLoop :: Emu ()
mainLoop = do
  q <- processEvents
  unless q (frame >> mainLoop)

frame :: Emu ()
frame = do
  fb <- gets frontBuffer
  bb <- gets backBuffer
  t1 <- liftIO $ getTime Monotonic
  replicateM_ 4000 cpuStep
  liftIO $ do
    unlockSurface bb
    blitSurface bb Nothing fb Nothing
    SDL.flip fb
    lockSurface bb
  t2 <- liftIO $ getTime Monotonic
  liftIO $ printf "\r%.2f FPS" ((1 :: Double) / (fromIntegral (nsec t2 - nsec t1) / 1e9))
  liftIO $ hFlush stdout
  vblank .= True

initState :: Assembly -> Surface -> Surface -> IO EmuState
initState Assembly { rom = rom, start = start } fb bb = do
  regs <- newArray (0x0, 0xf) 0
  mem <- newListArray (0x0000, 0xFFFF) (BS.unpack rom ++ replicate (0x10000 - BS.length rom) 0)
  ot <- genOps
  sd <- initSound

  return EmuState
    { _pc = start
    , _sp = 0xFDF0
    , _flags = 0x0
    , _spriteFlip = ( False, False )
    , _spriteSize = ( 0, 0 )
    , _bgc = 0
    , _vblank = False
    , _palette = defaultPalette
    , sound = sd
    , _tone = Simple
    , frontBuffer = fb
    , backBuffer = bb
    , regs = regs
    , memory = mem
    , opTable = ot
    }

cpuStep = {-# SCC "cpuStep" #-} do
  p <- use pc
  pc .= p + 4
  (oc : ib) <- forM [0..3] (\o -> load8 (Mem (p + o)))  
  ot <- gets opTable
  i <- liftIO (readArray ot oc)
  case i of
    Nothing -> debug $ printf "<unimplemented %02x>" oc
    Just (Instruction { parser = p, exec = e, printer = pr }) -> do
      let as = p ib
--      liftIO $ putStrLn $ runIdentity $ pr as
      e as

defaultPalette = map toColor (
  [ 0x000000 -- (Black, Transparent in foreground layer)
  , 0x000000 -- (Black)
  , 0x888888 -- (Gray)
  , 0xBF3932 -- (Red)
  , 0xDE7AAE -- (Pink)
  , 0x4C3D21 -- (Dark brown)
  , 0x905F25 -- (Brown)
  , 0xE49452 -- (Orange)
  , 0xEAD979 -- (Yellow)
  , 0x537A3B -- (Green)
  , 0xABD54A -- (Light green)
  , 0x252E38 -- (Dark blue)
  , 0x00467F -- (Blue)
  , 0x68ABCC -- (Light blue)
  , 0xBCDEE4 -- (Sky blue)
  , 0xFFFFFF -- (White)
  ] :: [ Word32 ]) where
    toColor x = Color (byte 2 x) (byte 1 x) (byte 0 x)