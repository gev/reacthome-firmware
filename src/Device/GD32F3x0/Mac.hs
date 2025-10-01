module Device.GD32F3x0.Mac where

import Data.Buffer
import Ivory.Language
import Support.ReadAddr

maxPrime48 :: Uint64
maxPrime48 = 0xFFFFFFFFFFC5

makeMac :: Buffer 6 Uint8 -> Ivory (ProcEffects s ()) ()
makeMac buff = do
    n1 <- readAddr64 0x1FFFF7E0 0x1FFFF7AC
    n2 <- readAddr64 0x1FFFF7B0 0x1FFFF7B4
    n <- local . ival $ n1 .^ n2
    arrayMap \ix -> do
        n' <- deref n
        store (buff ! ix) (castDefault $ n' .& 0xFF)
        store n $ n' `iShiftR` 8
  where
    castAddr64u a = (safeCast @Uint32 @Uint64) <$> readAddr32u a
    readAddr64 a1 a2 = do
        a1' <- castAddr64u a1
        a2' <- castAddr64u a2
        pure $ (a1' `iShiftL` 32 .| a2') .% maxPrime48
