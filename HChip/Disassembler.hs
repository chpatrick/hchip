{-# LANGUAGE GADTs #-}

module HChip.Disassembler where

import Data.Array
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.List
import Data.Word
import System.Environment
import Text.Printf

import HChip.CPU
import HChip.Loader
import HChip.Util

main' f = do
  rom <- BSL.readFile f
  case loadAssembly rom of
  	Left e -> putStrLn ("Problem parsing ROM: " ++ e)
  	Right (Assembly (major, minor) pc rom) -> do
  		printf "Version: %d.%d\n" major minor
  		printf "Start address: %04x\n" pc
  		disasmAll 0 rom

showAll :: Args ts -> [ String ]
showAll (x ::: xs) = disasm x : showAll xs
showAll Nil = []

disassemble :: Instruction -> InstructionBytes -> String
disassemble Instruction { mnemonic = m, parser = p } ib
  = m ++ " " ++ (intercalate ", " $ showAll $ p ib)

disasmAll :: Word16 -> BS.ByteString -> IO ()
disasmAll a r = do
	let ( instr, next ) = BS.splitAt 4 r
	if BS.length instr < 4 then return () else do
		let Just ( opcode, body ) = BS.uncons instr
		i <- ops !? opcode
		let d = case i of
			Nothing -> "<unimplemented %02x>"
			Just i -> disassemble (ops ! opcode) (BS.unpack body)
		printf "%04x: %s\n" a d
		disasmAll (a + 4) next

main = do
	fs <- getArgs
	case fs of
		[ f ] -> main' f
		_ -> putStrLn "Usage: hchipdasm <rom>"