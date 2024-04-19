{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Hexdim.Prim where

import Clash.Prelude

import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.RWS
import Data.Monoid.Generic
import Data.Maybe

type Wire = Unsigned 8
type Reg = Unsigned 2
type Imm = Unsigned 4

data PipeR = PipeR
  { _cycle0       :: Bool

  , _fromInstrMem :: Wire
  , _fromDataMem  :: Wire
  , _fromIO       :: Wire
  , _fromRegs1    :: Wire
  , _fromRegs2    :: Wire }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)
makeLenses ''PipeR

data PipeW = PipeW
  { _addrInstrMem :: First Wire
  
  , _addrDataMem  :: First Wire
  , _toDataMem    :: First (Maybe Wire)
  
  , _addrIO       :: First Wire
  , _toIO         :: First (Maybe Wire)

  , _selRegs1     :: First Reg
  , _selRegs2     :: First Reg
  , _selRegs      :: First Reg
  , _toRegs       :: First (Maybe Wire) }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)
  deriving Semigroup via GenericSemigroup PipeW
  deriving Monoid via GenericMonoid PipeW
makeLenses ''PipeW

data OpMA -- Address Data
  = IORead   Wire
  | IOWrite  Wire    Wire
  | MemRead  Wire
  | MemWrite Wire    Wire
  | MANop
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)

data OpEX -- Reg1 Reg2
  = ALUAdd   Wire Wire
  | ALUNand  Wire Wire
  | ALUXor   Wire Wire
  | ALUFlip  Wire
  | ALUComp  Wire
  | ALUShr   Wire
  | ALUSend  Wire
  | JMP      Wire
  | JZ       Wire
  | JO       Wire
  | EXNop
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)

data OpWB
  = WBImm Reg
  | WBMA  Reg
  | WBEX  Reg
  | WBNop
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)

data Instruction = Instruction
  { _imm :: Imm
  , _opMA :: OpMA
  , _opEX :: OpEX
  , _opWB :: OpWB }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)
makeLenses ''Instruction

data StageIFS = StageIFS
  { _pc :: Wire }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)
makeLenses ''StageIFS

type StageIDS = ()

type StageMAS = ()

data StageEXS = StageEXS
  { _isZero     :: Bool
  , _isOverflow :: Bool }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)
makeLenses ''StageEXS

type StageWBS = ()

type StoreIFID = Wire

type StoreIDMA = Instruction

type StoreMAEX = ()

type StoreEXWB = ()

data PipeS = PipeS
  { _stageIFS :: StageIFS
  , _stageIDS :: StageIDS
  , _stageMAS :: StageMAS
  , _stageEXS :: StageEXS
  , _stageWBS :: StageWBS
  , _storeIFID :: StoreIFID
  , _storeIDMA :: StoreIDMA
  , _storeMAEX :: StoreMAEX
  , _storeEXWB :: StoreEXWB }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)
makeLenses ''PipeS

initState :: PipeS
initState = PipeS
  { _stageIFS = StageIFS
    { _pc = 0 }
  , _stageIDS = ()
  , _stageMAS = ()
  , _stageEXS = StageEXS
    { _isZero = False
    , _isOverflow = False }
  , _stageWBS = ()
  , _storeIFID = 0
  , _storeIDMA = Instruction
    { _imm = 0
    , _opMA = MANop
    , _opEX = EXNop
    , _opWB = WBNop }
  , _storeMAEX = ()
  , _storeEXWB = () }

type Pipe s m a = RWST PipeR PipeW s m a

stageIF :: Monad m => () -> Pipe StageIFS m StoreIFID
stageIF () = do
  curpc <- use pc
  scribe addrInstrMem (pure curpc)
  pc %= (+ 1)
  ifM (view cycle0) (return 0) $ (view fromInstrMem)

stageID :: Monad m => StoreIFID -> Pipe StageIDS m StoreIDMA
stageID = \case
  -- NOP
  $(bitPattern "00__00__") -> return Instruction
    { _imm = 0, _opMA = MANop, _opEX = EXNop, _opWB = WBNop }
  -- JMP JZ JO
  $(bitPattern "00rr01__") -> unOP rr (const MANop) JMP (const WBNop)
  $(bitPattern "00rr10__") -> unOP rr (const MANop) JZ (const WBNop)
  $(bitPattern "00rr11__") -> unOP rr (const MANop) JO (const WBNop)
  -- STR LOAD OUT IN
  $(bitPattern "01rr00ss") ->
    biOP rr ss (flip MemWrite) (const2 EXNop) (const WBNop)
  $(bitPattern "01rr01ss") ->
    biOP rr ss (const MemRead) (const2 EXNop) WBMA
  $(bitPattern "01rr10ss") ->
    biOP rr ss (flip IOWrite) (const2 EXNop) (const WBNop)
  $(bitPattern "01rr11ss") ->
    biOP rr ss (const IORead) (const2 EXNop) WBMA
  -- SETL
  $(bitPattern "10rrmmmm") -> return Instruction
    { _imm = (bitCoerce mmmm)
    , _opMA = MANop
    , _opEX = EXNop
    , _opWB = WBImm (bitCoerce rr) }
  -- ADD NAND XOR FLIP COMP SHR SEND
  $(bitPattern "11rr00ss") -> biOP rr ss (const2 MANop) ALUAdd WBEX
  $(bitPattern "11rr01ss") -> biOP rr ss (const2 MANop) ALUNand WBEX
  $(bitPattern "11rr10ss") -> biOP rr ss (const2 MANop) ALUXor WBEX
  $(bitPattern "11rr1100") -> unOP rr (const MANop) ALUFlip WBEX
  $(bitPattern "11rr1101") -> unOP rr (const MANop) ALUComp WBEX
  $(bitPattern "11rr1110") -> unOP rr (const MANop) ALUShr WBEX
  $(bitPattern "11rr1111") -> unOP rr (const MANop) ALUSend WBEX
  -- Should not happen, only to dismiss the non-exhaustive warning
  _ -> return Instruction
    { _imm = 0, _opMA = MANop, _opEX = EXNop, _opWB = WBNop }
  where
    const2 :: x -> a -> b -> x
    const2 = const . const
    
    unOP :: Monad m => BitVector 2
         -> (Wire -> OpMA) -> (Wire -> OpEX)
         -> (Reg -> OpWB) -> Pipe StageIDS m Instruction
    unOP rr fma fex fwb = do
      let r = bitCoerce rr
      scribe selRegs1 (pure r)
      regs1 <- view fromRegs1
      return Instruction
        { _imm = 0
        , _opMA = fma regs1
        , _opEX = fex regs1
        , _opWB = fwb r }

    biOP :: Monad m => BitVector 2 -> BitVector 2
         -> (Wire -> Wire -> OpMA) -> (Wire -> Wire -> OpEX)
         -> (Reg -> OpWB) -> Pipe StageIDS m Instruction
    biOP rr ss fma fex fwb = do
      let r1 = bitCoerce rr
          r2 = bitCoerce ss
      scribe selRegs1 (pure r1)
      scribe selRegs2 (pure r2)
      regs1 <- view fromRegs1
      regs2 <- view fromRegs2
      return Instruction
        { _imm = 0
        , _opMA = fma regs1 regs2
        , _opEX = fex regs1 regs2
        , _opWB = fwb r1 }

stageMA :: Monad m => StoreIDMA -> Pipe StageMAS m StoreMAEX
stageMA = undefined

stageEX :: Monad m => StoreMAEX -> Pipe StageEXS m StoreEXWB
stageEX = undefined

stageWB :: Monad m => StoreEXWB -> Pipe StageWBS m ()
stageWB = undefined

store :: Monad m => a -> Pipe a m a
store x = get >>= \x0 -> put x >> return x0

pipeM :: Monad m => () -> Pipe PipeS m ()
pipeM =                            (zoom stageIFS . stageIF)
  >=> (zoom storeIFID . store) >=> (zoom stageIDS . stageID)
  >=> (zoom storeIDMA . store) >=> (zoom stageMAS . stageMA)
  >=> (zoom storeMAEX . store) >=> (zoom stageEXS . stageEX)
  >=> (zoom storeEXWB . store) >=> (zoom stageWBS . stageWB)

-- Miscellaneous
fromFirst :: a -> First a -> a
fromFirst x = fromMaybe x . getFirst
