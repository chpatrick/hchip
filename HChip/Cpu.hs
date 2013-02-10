{-# LANGUAGE DataKinds, GADTs, TypeOperators, KindSignatures, TypeFamilies, ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module HChip.CPU where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Data.Array hiding ((//))
import Data.Bits
import Data.List
import Data.Word
import Text.Printf

import HChip.Machine
import HChip.Util

-- CRAZY TYPE LEVEL STUFF

data Args ts where
  Nil :: Args '[]
  (:::) :: Disasm a => a -> Args ts -> Args (a ': ts)
  -- the Disasm context here is a bit of a hack

-- type level concat
type family (:+:) (l :: [*]) (k :: [*]) :: [*]
type instance '[] :+: k = k
type instance (a ': l) :+: k = a ': (l :+: k)

infixr 4 :::

type InstructionBytes = [ Word8 ]
type InstructionParser ts = InstructionBytes -> Args ts
type UnaryParser a = InstructionParser '[a]

type family Input f :: [*]
type instance Input (a -> f) = a ': Input f
type instance Input (Emu r) = '[]

type family Output f :: *
type instance Output (a -> f) = Output f
type instance Output (Emu r) = r

class (i ~ Input f, o ~ Output f) => ConvertArgs f i o where
  toArgs :: f -> Args i -> Emu o
 
instance (ConvertArgs f i o) => ConvertArgs (a -> f) (a ': i) o where
  toArgs f (a ::: as) = toArgs (f a) as
 
instance ConvertArgs (Emu o) '[] o where
  toArgs e Nil = e

(+++) :: Args ts -> Args ts' -> Args (ts :+: ts')
Nil +++ ts = ts
(t ::: ts) +++ ts' = t ::: (ts +++ ts')

(//) :: InstructionParser ts -> InstructionParser ts' -> InstructionParser (ts :+: ts')
(//) p p' i
  = p i +++ p' i

nullary :: InstructionParser '[]
nullary = const Nil

-- INSTRUCTION PARSERS

nibbleP :: Int -> UnaryParser Word8
nibbleP n i
  = (if n .&. 1 == 1 then lowNibble else highNibble) (i !! (n `shiftR` 1)) ::: Nil

x = nibbleP 1
y = nibbleP 0

imm :: UnaryParser Word16
imm i = buildInt (tail i) ::: Nil
mapArg :: Disasm b => (a -> b) -> Args (a ': '[]) -> Args (b ': '[])
mapArg f (v ::: Nil) = (f v ::: Nil)

r :: UnaryParser Word8 -> UnaryParser Register
r p = mapArg Reg . p

c :: Disasm a => a -> UnaryParser a
c x = const (x ::: Nil)

mem :: UnaryParser Memory
mem = mapArg Mem .imm

-- DISASSEMBLY

class Disasm a where
  disasm :: a -> String

instance Disasm Word8 where
  disasm = show

instance Disasm Word16 where
  disasm = printf "#%04x"

instance Disasm Register where
  disasm (Reg n) = printf "r%x" n

instance Disasm SP where
  disasm SP = "SP"

instance Disasm Memory where
  disasm (Mem a) = printf "#%04x" a

showAll :: Args ts -> [ String ]
showAll (x ::: xs) = disasm x : showAll xs
showAll Nil = []

disassemble :: Instruction -> InstructionBytes -> String
disassemble Instruction { mnemonic = m, parser = p } ib
  = m ++ " " ++ (intercalate ", " $ showAll $ p ib)

-- INSTRUCTIONS

data Instruction = forall ts. Instruction
  { mnemonic :: String 
  , parser   :: InstructionParser ts
  , exec     :: Args ts -> Emu ()
  }

i o m p e = ( o,  Instruction m p (toArgs e) )

-- OP TABLE

ops :: Array Word8 Instruction
ops = array (0x00, 0xD1)
  [ i 0x00 "NOP" nullary nop
  , i 0x07 "RND" (r x // imm) (\rx max -> getRandomR (0, max + 1) >>= save16 rx)

  , i 0x10 "JMP" imm jmp
  , i 0x13 "JME" (r x // r y // imm) $ \rx ry a -> do
    vx <- load16 rx
    vy <- load16 ry
    when (vx == vy) (jmp a)
  , i 0x16 "JMP" (r x) (load16 >=> jmp)

  , i 0x14 "CALL" imm call
  , i 0x15 "RET" nullary (pop >>= jmp)
  , i 0x18 "RET" (r x) (load16 >=> call)

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
  ]

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
