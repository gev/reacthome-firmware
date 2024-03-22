{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Display.Canvas1D where

import           Control.Monad.State
import           Core.Context
import           Data.Color
import           Data.Value
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Proxy
import           Support.Cast



newtype Canvas1D (n :: Nat) = Canvas1D {runCanvas :: RunValues Uint8 }



mkCanvas1D :: forall m n. (MonadState Context m, KnownNat n)
           => String -> m (Canvas1D n)
mkCanvas1D id = do
    let runCanvas = runValues id $ replicate size 0
    runCanvas addArea
    pure $ Canvas1D runCanvas
    where size = 3 * fromInteger (fromTypeNat (aNat :: NatType n))



clearCanvas :: KnownNat n
            => Canvas1D n -> Ivory (ProcEffects s ()) ()
clearCanvas Canvas1D{..} =
    runCanvas $ \canvas ->
        arrayMap $ \ix -> store (addrOf canvas ! ix) 0



writePixel :: KnownNat n
           => Canvas1D n -> Ix n -> Ref s1 (Struct RGB)
           -> Ivory ('Effects (Returns ()) r (Scope s2)) ()
writePixel Canvas1D{..} ix pixel =
    runCanvas $ \canvas ->
        set canvas 0 g >> set canvas 1 r >> set canvas 2 b
    where cast color = castFloatToUint8 . (255 *) =<< deref (pixel ~> color)
          set canvas j color = store (addrOf canvas ! toIx (3 * fromIx ix + j)) =<< cast color



writePixels :: KnownNat n
            => Canvas1D n -> Ref s1 (Array n (Struct RGB))
            -> Ivory ('Effects (Returns ()) r (Scope s2)) ()
writePixels canvas pixels =
    arrayMap $ \ix -> writePixel canvas ix (pixels ! ix)
