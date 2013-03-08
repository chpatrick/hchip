module HChip.Debug where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Text.Printf

import HChip.Machine

dumpState :: Emu ()
dumpState = do
	forM_ [0x0..0xF] $ \r -> do
		v <- load16 (Reg r)
		liftIO $ printf "R%X: %d " r v
	liftIO $ putStrLn ""
	z <- use zero
	when z $ liftIO $ putStr "ZERO "
	n <- use negative
	when n $ liftIO $ putStr "NEG "
	c <- use carry
	when c $ liftIO $ putStr "CARRY "
	o <- use overflow
	when o $ liftIO $ putStr "OVRF "
	liftIO $ putStrLn ""