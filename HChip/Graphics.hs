module HChip.Graphics where

import Data.Word
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import Foreign
import Foreign.Marshal.Array
import Graphics.UI.SDL
import Graphics.UI.SDL.Video

import HChip.Machine
import HChip.Util

cls :: Emu ()
cls = do
	s <- gets surface
	liftIO $ fillRect s Nothing (Pixel 0)
	return ()

setPixel :: Ptr Word8 -> Int -> Int -> Int -> Word8 -> Emu ()
setPixel scan0 pitch x y c
  = when (x >= 0 && x < 320 && y >= 0 && y < 240 && c /= 0) $ do
		let a = scan0 `plusPtr` (y * pitch + x)
		cp <- liftIO $ peek a
		when (cp /= (0 :: Word8)) (carry .= True) -- no idea why haskell can't infer this
		liftIO $ poke a c

flipCoord f m v
  = if f then m - v - 1 else v

drw :: Register -> Register -> Word16 -> Emu ()
drw rx ry a = do
	x0 <- sign <$> load16 rx
	y0 <- sign <$> load16 ry
	( w, h ) <- use spriteSize
	s <- gets surface
	scan0 <- castPtr <$> liftIO (surfaceGetPixels s)
	let pitch = fromIntegral $ surfaceGetPitch s
	( fh, fv ) <- use spriteFlip
	carry .= False
	forM_ [0 .. h - 1] $ \y -> do
		forM [0 .. w - 1] $ \x -> do
			pp <- load8 (Mem (a + fromIntegral y * fromIntegral w + fromIntegral x))
			let sx1 = flipCoord fh (fromIntegral w * 2) (fromIntegral x * 2) + fromIntegral x0
			let sx2 = flipCoord fh (fromIntegral w * 2) (fromIntegral x * 2 + 1) + fromIntegral x0
			let sy = fromIntegral (flipCoord fv h y) + fromIntegral y0
			setPixel scan0 pitch sx1 sy (highNibble pp)
			setPixel scan0 pitch sx2 sy (highNibble pp)

pal :: Word16 -> Emu ()
pal a = do
	cs <- forM [0..15] $ \n -> do
		[ r, g, b ] <- forM [0..2] (\c -> load8 (Mem (a + n * 3 + c)))
		return (Color r g b)
	s <- gets surface
	liftIO $ setColors s cs 0
	return ()