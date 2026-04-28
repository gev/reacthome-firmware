module Data.Util where

import Data.Bits
import Data.Word

unPack16BE :: Word16 -> [Word8]
unPack16BE w =
    [ fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

unPack32BE :: Word32 -> [Word8]
unPack32BE w =
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]
