{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

module Interface.NeoPixel where

import           Data.Record
import           Ivory.Language


type RGB = "rgb_struct"

[ivory|
    struct rgb_struct
    { r :: Uint8
    ; g :: Uint8
    ; b :: Uint8
    }
|]


class NeoPixel p where
    sendPixels :: PixelBuffer t => p -> Records n t -> Ivory eff ()


class PixelBuffer t where
    setPixel :: Records n t -> Ix n -> Record t -> Ivory eff ()
