module HChip where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Lens
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Trans
import Data.Array.IO
import Data.Word
import Graphics.UI.SDL as SDL
import Text.Printf
import System.Random

import HChip.Loader
import HChip.Machine
import HChip.CPU
import HChip.Debug
import HChip.Ops
import HChip.Util

main' f = do
  rom <- BSL.readFile f
  case loadAssembly rom of
    Left e -> putStrLn ("Problem parsing ROM: " ++ e)
    Right a -> do 
      surf <- initSDL
      s <- initState a surf
      rng <- getStdGen
      evalStateT (evalRandT (runEmu mainLoop) rng) s
      quit

initSDL :: IO Surface
initSDL = do 
  SDL.init [ InitVideo ]
  s <- setVideoMode 320 240 8 [ HWSurface, DoubleBuf, HWPalette ]
  setColors s defaultPalette 0
  return s

processEvents :: Emu Bool
processEvents = do
  e <- liftIO $ pollEvent
  ( q, l ) <- case e of
    Quit -> return ( True, False )
    NoEvent -> return ( False, False )
    _ -> return ( False, True )
  if l then processEvents else return q

mainLoop :: Emu ()
mainLoop = do
  q <- processEvents
  if q then return () else frame >> mainLoop

frame :: Emu ()
frame = do
  s <- gets surface
  replicateM_ 1000 cpuStep
  liftIO $ SDL.flip s
  vblank .= True

initState :: Assembly -> Surface -> IO EmuState
initState Assembly { rom = rom, start = start } s = do
	regs <- newArray (0x0, 0xf) 0
	mem <- newListArray (0x0000, 0xFFFF) (BS.unpack rom ++ replicate (0x10000 - BS.length rom) 0)

	return EmuState
		{ _pc = start
		, _sp = 0xFDF0
		, _flags = 0x0
    , _spriteFlip = ( False, False )
    , _spriteSize = ( 0, 0 )
    , _bgc = 0
    , _vblank = False
    , surface = s
		, regs = regs
		, memory = mem
		}

cpuStep = do
  p <- use pc
  pc += 4
  (oc : ib) <- forM [0..3] (\o -> load8 (Mem (p + o)))	
  i <- liftIO (ops !? oc)
  case i of
    Nothing -> debug $ printf "<unimplemented %02x>" oc
    Just (Instruction { parser = p, exec = e, printer = pr }) -> do
      let as = p ib
--      liftIO $ putStrLn $ runIdentity $ pr as
--      liftIO $ getChar
      e as
--      dumpState
--      liftIO $ putStrLn ""

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
    toColor x = Color (byte 0 x) (byte 1 x) (byte 2 x)