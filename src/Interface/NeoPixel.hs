{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interface.NeoPixel where

import           Data.Record
import           GHC.TypeNats
import           Ivory.Language




class NeoPixelBuffer b => NeoPixel p b where
    sendPixels :: p -> b -> Ivory eff ()


class NeoPixelBuffer t where
    setByte :: t -> Ix n -> Uint8 -> Ivory eff ()
