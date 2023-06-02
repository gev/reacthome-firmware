{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.NeoPixel.Buffer where

import           GHC.TypeNats
import           Ivory.Language



class NeoPixelBuffer b where
    clearByte   :: KnownNat n
                => b n
                -> Ix n
                -> Ivory (AllowBreak (ProcEffects s ())) ()

    writeByte   :: KnownNat n
                => b n
                -> Ix n
                -> Uint8
                -> Ivory (AllowBreak (ProcEffects s ())) ()

    clearBuffer :: KnownNat n
                => b n
                -> Ivory (ProcEffects s ()) ()
    clearBuffer buff = arrayMap $ \ix -> clearByte buff ix
