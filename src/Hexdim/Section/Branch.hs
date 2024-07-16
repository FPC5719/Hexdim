module Hexdim.Section.Branch(Branch) where

import Hexdim.Datatype
import Hexdim.Section

import Clash.Prelude
import Control.Lens(view, set, (^.), (&))
import Data.Maybe
import Data.Monoid

data Branch

data BranchInstr = Nop | Jmp | Jz | Jo
  deriving (Generic, NFDataX)

instance Section Branch where
  type Decoded Branch = (BranchInstr, Addr)
  type Buffer Branch = ()

  decoder rs = \case
    $(bitPattern "00rrcc00") -> Just $
      let r = bitCoerce rr :: RegSel
          dst = rs !! r
      in case cc of
        0b00 -> (Nop, 0)
        0b01 -> (Jmp, dst)
        0b10 -> (Jz,  dst)
        _    -> (Jo,  dst)
    _ -> Nothing

  onDecode fwd (op, dst) = do
    f <- view statusR
    let (fz, fo) = fromMaybe f $ getFirst (fwd ^. fStatus)
    let jmp = def & set fTarget (pure dst)
    case op of
      Nop -> pure ((), def)
      Jmp -> pure ((), jmp)
      Jz -> pure ((), if fz then jmp else def)
      Jo -> pure ((), if fo then jmp else def)

  onExecute () = pure (Nothing, def)
