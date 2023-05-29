{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interface.NeoPixel where

import           Data.NeoPixel.Buffer
import           Ivory.Language


class NeoPixelBuffer t n => NeoPixel p t n where
    transmitPixels :: p -> t n -> Ivory eff ()
