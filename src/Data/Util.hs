module Data.Util where

import Data.Bits
import Data.Word

unPack16BE :: Word16 -> [Word8]
unPack16BE w =
    [ fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]