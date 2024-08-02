module Hexdim.ROM where

import Hexdim.Datatype

import Clash.Prelude
import Clash.Prelude.ROM.File

romPath :: String
romPath = "ROMFILE"

writeROM :: [Value] -> IO ()
writeROM xs = writeFile romPath (memFile Nothing xs)
