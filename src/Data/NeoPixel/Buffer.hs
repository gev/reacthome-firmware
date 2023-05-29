{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.NeoPixel.Buffer where

import           GHC.TypeNats
import           Ivory.Language


type Px n = Ix (n * 1)


class NeoPixelBuffer n t where
    writeByte :: t n -> Px n -> Uint8 -> Ivory eff ()
