module Hexdim.Section.Memory(Memory) where

import Hexdim.Datatype
import Hexdim.Section

import Clash.Prelude
import Control.Lens(scribe, view)

data Memory

data MemoryInstr
  = Store Addr Value
  | Load  Addr RegSel
  | Out   Addr Value
  | In    Addr RegSel
  deriving (Generic, NFDataX)

data MemoryAccess
  = None
  | FromMem RegSel
  | FromPeri RegSel
  deriving (Show, Generic, NFDataX)

instance Section Memory where
  type Decoded Memory = MemoryInstr
  type Buffer Memory = MemoryAccess

  decoder rs = \case
    $(bitPattern "01rrccss") -> Just $
      let r1 = bitCoerce rr :: RegSel
          r2 = bitCoerce ss :: RegSel
      in case cc of
        0b00 -> Store (rs !! r2) (rs !! r1)
        0b01 -> Load  (rs !! r2) r1
        0b10 -> Out   (rs !! r2) (rs !! r1)
        _    -> In    (rs !! r2) r1
    _ -> Nothing

  onDecode fwd = \case
    Store addr val -> do
      scribe memoryA . pure $ addr
      scribe memoryW . pure $ val
      pure (None, def)
    Load addr dst -> do
      scribe memoryA . pure $ addr
      pure (FromMem dst, def)
    Out addr val -> do
      scribe periA . pure $ addr
      scribe periW . pure $ val
      pure (None, def)
    In addr dst -> do
      scribe periA . pure $ addr
      pure (FromPeri dst, def)

  onExecute = \case
    None -> pure (Nothing, def)
    FromMem dst -> do
      val <- view memoryR
      pure (Just (dst, val), def)
    FromPeri dst -> do
      val <- view periR
      pure (Just (dst, val), def)
