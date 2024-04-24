{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}

module Data.Display.Canvas1D where

import           Control.Monad.State
import           Core.Context
import           Data.Color
import           Data.Value
import           GHC.Arr              (array)
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Proxy
import           Support.Cast




type Canvas1DSize n = n + n + n


newtype Canvas1D n = Canvas1D
    { canvas :: Values (Canvas1DSize n) Uint8
    }



mkCanvas1D :: Values (Canvas1DSize n) Uint8 -> Canvas1D n
mkCanvas1D = Canvas1D



clearCanvas :: KnownNat (Canvas1DSize n)
            => Canvas1D n -> Ivory (ProcEffects s ()) ()
clearCanvas Canvas1D{..} =
    arrayMap $ \ix -> store (canvas ! ix) 0



writePixel :: (KnownNat n, KnownNat (Canvas1DSize n))
           => Canvas1D n -> Ix n -> Ref s1 (Struct RGB)
           -> Ivory ('Effects (Returns ()) r (Scope s2)) ()
writePixel Canvas1D{..} ix pixel =
    set 0 g >> set 1 r >> set 2 b
    where cast color = castFloatToUint8 . (255 *) =<< deref (pixel ~> color)
          set j color = store (canvas ! toIx (3 * fromIx ix + j)) =<< cast color



writePixels :: (KnownNat n, KnownNat (Canvas1DSize n))
            => Canvas1D n -> Ref s1 (Array n (Struct RGB))
            -> Ivory ('Effects (Returns ()) r (Scope s2)) ()
writePixels canvas pixels =
    arrayMap $ \ix -> writePixel canvas ix (pixels ! toIx ix)
