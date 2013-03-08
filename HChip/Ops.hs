module HChip.Ops (ops) where

import Data.Array hiding ((//))
import Data.Word
import Control.Lens
import Control.Monad
import Control.Monad.Random
import HChip.Machine
import HChip.CPU
import HChip.ALU
import HChip.Graphics

ops :: Array Word8 Instruction
ops = array (0x00, 0xD1) (
  [ i 0x00 "NOP" nullary nop
  , i 0x01 "CLS" nullary cls
  , i 0x02 "VBLNK" nullary $ do
    v <- use vblank
    if v then vblank .= False else pc -= 4
  , i 0x03 "BGC" z (assign bgc)
  , i 0x04 "SPR" (ll // hh) (\w h -> spriteSize .= ( w, h ))
  , i 0x05 "DRW" (r x // r y // imm) drw
  , i 0x06 "DRW" (r x // r y // r z) (\x y -> load16 >=> drw x y)
  , i 0x07 "RND" (r x // imm) (\rx m -> getRandomR (0, m + 1) >>= save16 rx)

  , i 0x08 "FLIP" (b 2 1 // b 2 0) (\h v -> spriteFlip .= ( h, v ))

  , u 0x09 "SND0" nullary
  , u 0x0A "SND1" imm
  , u 0x0B "SND2" imm
  , u 0x0C "SND3" imm
  , u 0x0D "SNP" (r x // imm)

  , u 0x0E "SNG" (ad // imm)

  , i 0x10 "JMP" imm jmp
  , p 0x12 (condPrinter 'J') (x // imm) (\c a -> whenC c (jmp a))
  , i 0x13 "JME" (r x // r y // imm) $ \rx ry a -> do
    vx <- load16 rx
    vy <- load16 ry
    when (vx == vy) (jmp a)
  , i 0x16 "JMP" (r x) (load16 >=> jmp)

  , i 0x14 "CALL" imm call
  , i 0x15 "RET" nullary (pop >>= jmp)
  , p 0x17 (condPrinter 'C') (x // imm) (\c a -> whenC c (call a))
  , i 0x18 "CALL" (r x) (load16 >=> call)

  , i 0x20 "LDI" (r x // imm) save16
  , i 0x21 "LDI" (c SP // imm) save16
  , i 0x22 "LDM" (r x // mem) ld
  , i 0x23 "LDM" (r x // r y) (\rx ry -> fmap Mem (load16 ry) >>= ld rx)
  , i 0x24 "MOV" (r x // r y) ld

  , i 0x30 "STM" (r x // mem) stm
  , i 0x31 "STM" (r x // r y) (\rx ry -> fmap Mem (load16 ry) >>= stm rx)
  
  , i 0xC0 "PUSH" (r x) (load16 >=> push)
  , i 0xC1 "POP" (r x) (\rx -> pop >>= save16 rx)
  -- TODO: very slight optimizations to these four
  , i 0xC2 "PUSHALL" nullary (forM_ [0x0..0xf] (\r -> load16 (Reg r) >>= push))
  , i 0xC3 "POPALL" nullary (forM_ [0xf,0xe..0x0] (\r -> pop >>= save16 (Reg r)))
  , i 0xC4 "PUSHF" nullary (fmap fromIntegral (load8 Flags) >>= push)
  , i 0xC5 "POPF" nullary (fmap fromIntegral pop >>= save8 Flags)

  , i 0xD0 "PAL" imm pal
  , i 0xD1 "PAL" (r x) (load16 >=> pal)
  ] ++ aluOps)

nop :: Emu ()
nop = return ()

jmp = save16 PC

call a = load16 PC >>= push >> jmp a

ld x y = load16 y >>= save16 x

stm = flip ld

push v = do
  sp <- load16 SP
  save16 (Mem sp) v
  save16 SP (sp + 2)

pop = do
  sp <- load16 SP
  r <- load16 (Mem (sp - 2))
  save16 SP (sp - 2)
  return r