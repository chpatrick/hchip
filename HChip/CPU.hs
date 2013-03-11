{-# LANGUAGE DataKinds, GADTs, TypeOperators, KindSignatures, TypeFamilies, ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, FunctionalDependencies, UndecidableInstances #-}
module HChip.CPU where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Identity
import Data.Array hiding ((//))
import Data.Bits
import Data.List
import Data.Word
import Text.Printf

import HChip.Machine
import HChip.Util

-- CRAZY TYPE LEVEL STUFF

-- type level concat
type family (:+:) (l :: [*]) (k :: [*]) :: [*]
type instance '[] :+: k = k
type instance (a ': l) :+: k = a ': (l :+: k)

type UnaryParser a = InstructionParser '[a]

type family Input f :: [*]
type instance Input (a -> f) = a ': Input f
type instance Input (Emu r) = '[]
type instance Input (Identity r) = '[]

type family Output f :: *
type instance Output (a -> f) = Output f
type instance Output (Emu r) = Emu r
type instance Output (Identity r) = Identity r

class (i ~ Input f, o ~ Output f) => ConvertArgs f i o | i o -> f where
  toArgs :: f -> Args i -> o
 
instance (ConvertArgs f i o) => ConvertArgs (a -> f) (a ': i) o where
  toArgs f (a ::: as) = toArgs (f a) as
 
instance ConvertArgs (Emu o) '[] (Emu o) where
  toArgs e Nil = e

instance ConvertArgs (Identity o) '[] (Identity o) where
  toArgs i Nil = i

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

y = nibbleP 0
x = nibbleP 1
z = nibbleP 3

ll :: UnaryParser Word8
ll i = (i !! 1) ::: Nil

hh :: UnaryParser Word8
hh i = (i !! 2 ) ::: Nil

imm :: UnaryParser Word16
imm i = buildInt (tail i) ::: Nil
mapArg :: (a -> b) -> Args (a ': '[]) -> Args (b ': '[])
mapArg f (v ::: Nil) = (f v ::: Nil)

r :: UnaryParser Word8 -> UnaryParser Register
r p = mapArg Reg . p

c :: a -> UnaryParser a
c x = const (x ::: Nil)

mem :: UnaryParser Memory
mem = mapArg Mem .imm

b :: Int -> Int -> UnaryParser Bool
b byte bit i = testBit (i !! byte) bit ::: Nil

ad :: UnaryParser Word8
ad i = (i !! 0) ::: Nil

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

instance Disasm Bool where
  disasm False = "0"
  disasm True  = "1"

class DisasmAll a where
  disasmAll :: a -> [ String ]

instance DisasmAll (Args '[]) where
  disasmAll = const []

instance (Disasm a, DisasmAll (Args as)) => DisasmAll (Args (a ': as)) where
  disasmAll (x ::: xs) = disasm x : disasmAll xs

disasmArgs :: DisasmAll a => a -> String
disasmArgs = intercalate ", " . disasmAll

defaultPrinter :: DisasmAll a => String -> a -> Identity String
defaultPrinter m as = return (m ++ " " ++ disasmArgs as)

condPrinter :: DisasmAll (Args ts) => Char -> Args (Word8 ': ts) -> Identity String
condPrinter m (c ::: as) = defaultPrinter (m : condName c) as

p o pr p e = ( o, Instruction p pr (toArgs e ) )
i o m p e = ( o, Instruction p (defaultPrinter m) (toArgs e) )
u o m p = ( o, Instruction p (defaultPrinter m) (\_ -> debug ("Unimplemented instruction " ++ m)) )

conds :: Array Word8 (String, Emu Bool)
conds = array (0x0, 0xF)
  [ ( 0x0, ( "Z",  z )) 
  , ( 0x1, ( "NZ", not <$> z ))
  , ( 0x2, ( "N",  n ))
  , ( 0x3, ( "NN", not <$> n))
  , ( 0x4, ( "P",  (&&) <$> (not <$> n) <*> (not <$> z)))
  , ( 0x5, ( "O",  o ))
  , ( 0x6, ( "NO", not <$> o ))
  , ( 0x7, ( "A",  (&&) <$> (not <$> c) <*> (not <$> z) ))
  , ( 0x8, ( "AE", not <$> c ))
  , ( 0x9, ( "B",  c ))
  , ( 0xA, ( "BE", (||) <$> c <*> z ))
  , ( 0xB, ( "G",  (&&) <$> ((==) <$> o <*> n) <*> (not <$> z) ))
  , ( 0xC, ( "GE", (==) <$> o <*> n ))
  , ( 0xD, ( "L",  (/=) <$> o <*> n ))
  , ( 0xE, ( "LE", (||) <$> ((/=) <$> o <*> n) <*> z ))
  , ( 0xF, ( "RES", debug "Use of reserved cond." >> return False ))
  ] where
    z = use zero
    n = use negative
    o = use overflow
    c = use carry

whenC c m = do
  let ( _, cc ) = conds ! c
  t <- cc
  when t m

condName c = let ( m, _ ) = conds ! c in m