{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}

module Interface.NeoPixel where

import           Control.Monad.Writer
import           Core.Context
import           Data.NeoPixel.Buffer
import           GHC.TypeNats
import           Ivory.Language


class NeoPixel p b | p -> b where
    neoPixelBuffer :: (KnownNat n, MonadWriter Context m)
                   => p -> String -> m (b n)
    transmitPixels :: KnownNat n => p -> b n -> Ivory eff ()
