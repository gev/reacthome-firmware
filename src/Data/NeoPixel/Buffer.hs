{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.NeoPixel.Buffer where

import           GHC.TypeNats
import           Ivory.Language



class NeoPixelBuffer t where
    clearByte   :: t
                -> Sint32
                -> Ivory eff ()

    writeByte   :: t
                -> Sint32
                -> Uint8
                -> Ivory ('Effects (Returns ()) b (Scope s)) ()
