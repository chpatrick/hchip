module HChip.Graphics where

import Data.Word
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Array.IO
import Data.IORef 
import Foreign
import Foreign.Marshal.Array
import Graphics.UI.SDL
import Graphics.UI.SDL.Video

import HChip.Machine
import HChip.Util

cls :: Emu ()
cls = do
  bb <- gets backBuffer
  liftIO $ fillRect bb Nothing (Pixel 0)
  return ()

flipCoord f m v
  = if f then m - v - 1 else v

drw :: Register -> Register -> Word16 -> Emu ()
drw rx ry a = do
  x0 <- sign <$> load16 rx
  y0 <- sign <$> load16 ry
  ( w, h ) <- use spriteSize
  s <- gets backBuffer
  scan0 <- castPtr <$> liftIO (surfaceGetPixels s)
  let pitch = fromIntegral $ surfaceGetPitch s
  ( fh, fv ) <- use spriteFlip
  mem <- gets memory
  c <- liftIO $ do
    cr <- newIORef False
    forM_ [ (x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1]] $ \( x, y ) -> do
      pp <- readArray mem (a + fromIntegral y * fromIntegral w + fromIntegral x)
      let sx1 = flipCoord fh (fromIntegral w * 2) (fromIntegral x * 2) + fromIntegral x0
      let sx2 = flipCoord fh (fromIntegral w * 2) (fromIntegral x * 2 + 1) + fromIntegral x0
      let sy = fromIntegral (flipCoord fv h y) + fromIntegral y0
      let setPixel sx c = when (sx >= 0 && sx < 320 && sy >= 0 && sy < 240 && c /= 0) $ do
			      let a = scan0 `plusPtr` (sy * pitch + sx)
			      cp <- peek a
			      when (cp /= (0 :: Word8)) (writeIORef cr True) -- no idea why haskell can't infer this
			      poke a c
      setPixel sx1 (highNibble pp)
      setPixel sx2 (lowNibble pp)
    readIORef cr
  carry .= c

updatePalette :: Emu ()
updatePalette = do
  b <- use bgc
  p <- use palette
  fb <- gets frontBuffer
  bb <- gets backBuffer
  liftIO $ setColors bb ((p !! fromIntegral b) : tail p) 0
  return ()

pal :: Word16 -> Emu ()
pal a = do
  cs <- forM [0..15] $ \n -> do
    [ r, g, b ] <- forM [0..2] (\c -> load8 (Mem (a + n * 3 + c)))
    return (Color r g b)
  palette .= cs
  updatePalette
