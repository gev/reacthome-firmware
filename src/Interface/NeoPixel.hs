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


data RenderNeoPixel p = RenderNeoPixel
    { neoPixel  :: p
    , frameRate :: Uint32
    , render    :: forall s. Ivory (ProcEffects s ()) ()
    }


class Handler RenderNeoPixel p => NeoPixel p b | p -> b where
    neoPixelBuffer :: (KnownNat n, MonadWriter Context m)
                   => p -> String -> m (b n)
    transmitPixels :: KnownNat n => p -> b n -> Ivory eff ()
