module HChip.Ops (genOps) where

import Data.Array hiding ((//))
import Data.Array.IO
import Data.Word
import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Random
import HChip.Machine
import HChip.CPU
import HChip.ALU
import HChip.Graphics
import HChip.Sound

genOps :: IO (IOArray Word8 (Maybe Instruction))
genOps = do
  ot <- newArray (0x00, 0xFF) Nothing
  forM_ ops (\(oc, i) -> oc `seq` i `seq` writeArray ot oc (Just i))
  return ot

ops :: [ ( Word8, Instruction ) ]
ops = (
  [ i 0x00 "NOP" nullary (return ())
  , i 0x01 "CLS" nullary cls
  , i 0x02 "VBLNK" nullary $ do
    v <- use vblank
    if v then vblank .= False else pc -= 4
  , i 0x03 "BGC" z (\b -> bgc .= b >> updatePalette)
  , i 0x04 "SPR" (ll // hh) (curry (assign spriteSize))
  , i 0x05 "DRW" (r x // r y // imm) drw
  , i 0x06 "DRW" (r x // r y // r z) (\x y -> load16 >=> drw x y)
  , i 0x07 "RND" (r x // imm) (\rx m -> getRandomR (0, m + 1) >>= save16 rx)

  , i 0x08 "FLIP" (b 2 1 // b 2 0) (curry (assign spriteFlip))

  , i 0x09 "SND0" nullary killSound
  , i 0x0A "SND1" imm (play 500)
  , i 0x0B "SND2" imm (play 1000)
  , i 0x0C "SND3" imm (play 1500)
  , i 0x0D "SNP" (r x // imm) (\rx t -> load16 rx >>= (`play`t))

  , i 0x0E "SNG" (ad // imm) sng

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
  , i 0x23 "LDM" (r x // r y) (\rx ry -> Mem <$> load16 ry >>= ld rx)
  , i 0x24 "MOV" (r x // r y) ld

  , i 0x30 "STM" (r x // mem) stm
  , i 0x31 "STM" (r x // r y) (\rx ry -> Mem <$> load16 ry >>= stm rx)
  
  , i 0xC0 "PUSH" (r x) (load16 >=> push)
  , i 0xC1 "POP" (r x) (\rx -> pop >>= save16 rx)
  -- TODO: very slight optimizations to these four
  , i 0xC2 "PUSHALL" nullary (forM_ [0x0..0xf] (\r -> load16 (Reg r) >>= push))
  , i 0xC3 "POPALL" nullary (forM_ [0xf,0xe..0x0] (\r -> pop >>= save16 (Reg r)))
  , i 0xC4 "PUSHF" nullary (fromIntegral <$> use flags >>= push)
  , i 0xC5 "POPF" nullary (fromIntegral <$> pop >>= assign flags)

  , i 0xD0 "PAL" imm pal
  , i 0xD1 "PAL" (r x) (load16 >=> pal)
  ] ++ aluOps)

jmp = assign pc

call a = use pc >>= push >> jmp a

ld x y = load16 y >>= save16 x

stm = flip ld

push v = do
  sp' <- use sp
  save16 (Mem sp') v
  sp .= sp' + 2

pop = do
  sp' <- subtract 2 <$> use sp
  r <- load16 (Mem sp')
  sp .= sp'
  return r
