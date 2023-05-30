{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interface.NeoPixel where

import           Data.NeoPixel.Buffer
import           Ivory.Language



class NeoPixel p where
    transmitPixels :: p -> Ivory eff ()
