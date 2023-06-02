{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.NeoPixel.Buffer where

import           GHC.TypeNats
import           Ivory.Language



class NeoPixelBuffer t where
    clearByte   :: KnownNat n
                => t n
                -> Ix n
                -> Ivory eff ()

    writeByte   :: KnownNat n
                => t n
                -> Ix n
                -> Uint8
                -> Ivory ('Effects (Returns ()) b (Scope s)) ()

    clearBuffer :: KnownNat n
                => t n
                -> Ivory (ProcEffects s ()) ()
    clearBuffer buff = arrayMap $ \ix -> clearByte buff ix
