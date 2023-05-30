{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interface.NeoPixel where

import           Control.Monad.Writer
import           Core.Context
import           Data.NeoPixel.Buffer
import           GHC.TypeNats
import           Ivory.Language


class KnownNat n => NeoPixel b n where
    neoPixelBuffer :: MonadWriter Context m => String -> m (b n)


class KnownNat n => NeoPixelTransmitter p b n where
    transmitPixels :: p -> b n -> Ivory eff ()
