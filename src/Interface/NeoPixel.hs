{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interface.NeoPixel where

import           Data.NeoPixel.Buffer
import           Ivory.Language


class NeoPixelBuffer n t => NeoPixel p n t where
    transmitPixels :: p -> t n -> Ivory eff ()
