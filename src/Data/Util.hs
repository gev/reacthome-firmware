
module Data.Util where
import Data.Word
import Data.Bits

unPack16BE :: Word16 -> [Word8]
unPack16BE w =
    [ fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]