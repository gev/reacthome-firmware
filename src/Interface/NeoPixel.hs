{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}

module Interface.NeoPixel where

import           Control.Monad.Writer
import           Core.Context
import           Core.Handler
import           Data.NeoPixel.Buffer
import           GHC.TypeNats
import           Ivory.Language


data HandleNeoPixel p = HandleNeoPixel
    { neoPixel :: p
    , handle   :: forall eff. Ivory eff ()
    }


class Handler HandleNeoPixel p => NeoPixel p b | p -> b where
    neoPixelBuffer :: (KnownNat n, MonadWriter Context m)
                   => p -> String -> m (b n)
    transmitPixels :: KnownNat n => p -> b n -> Ivory eff ()
