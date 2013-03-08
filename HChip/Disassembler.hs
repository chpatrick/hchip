{-# LANGUAGE GADTs #-}

module HChip.Disassembler where

import Control.Monad
import Control.Monad.Identity
import Data.Array
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.List
import Data.Word
import System.Environment
import Text.Printf

import HChip.CPU
import HChip.Ops
import HChip.Loader
import HChip.Util

main' f = do
  rom <- BSL.readFile f
  case loadAssembly rom of
    Left e -> putStrLn ("Problem parsing ROM: " ++ e)
    Right (Assembly (major, minor) pc rom) -> do
      printf "Version: %d.%d\n" major minor
      printf "Start address: %04x\n" pc
      disasmLoop 0 rom

disassemble :: Instruction -> InstructionBytes -> String
disassemble Instruction { parser = p, printer = pr } ib
  = runIdentity $ pr $ p ib

disasmLoop :: Word16 -> BS.ByteString -> IO ()
disasmLoop a r = do
  let ( instr, next ) = BS.splitAt 4 r
  when (BS.length instr > 4) $ do
    let Just ( opcode, body ) = BS.uncons instr
    i <- ops !? opcode
    let d = case i of {
      Nothing -> printf "<unimplemented %02x>" opcode;
      Just i -> disassemble (ops ! opcode) (BS.unpack body)
    	}
    printf "%04x: %s\n" a d
    disasmLoop (a + 4) next

main = do
  fs <- getArgs
  case fs of
    [ f ] -> main' f
    _ -> putStrLn "Usage: hchipdasm <rom>"