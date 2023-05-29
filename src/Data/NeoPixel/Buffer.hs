{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.NeoPixel.Buffer where

import           GHC.TypeNats
import           Ivory.Language



class NeoPixelBuffer t n where
    writeByte :: KnownNat n
              => t n
              -> Ix n
              -> Uint8
              -> Ivory (AllowBreak (ProcEffects s ())) ()
