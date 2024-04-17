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



data Canvas1D (n :: Nat) = Canvas1D
    { runCanvas :: RunValues Uint8
    , offset    :: Sint32
    }



mkCanvas1D :: RunValues Uint8 -> Sint32  -> Canvas1D n
mkCanvas1D = Canvas1D



clearCanvas :: forall n s. KnownNat n => Canvas1D n -> Ivory (ProcEffects s ()) ()
clearCanvas Canvas1D{..} = do
    let size = fromInteger $ fromTypeNat (aNat :: NatType n) :: Ix n
    runCanvas $ \canvas ->
        upTo (toIx offset) (toIx $ offset + fromIx size) $ \ix ->
            store (addrOf canvas ! ix) 0



writePixel :: KnownNat n => Canvas1D n -> Ix n -> Ref s1 (Struct RGB)
           -> Ivory ('Effects (Returns ()) r (Scope s2)) ()
writePixel Canvas1D{..} ix pixel =
    runCanvas $ \canvas ->
        set canvas 0 g >> set canvas 1 r >> set canvas 2 b
    where cast color = castFloatToUint8 . (255 *) =<< deref (pixel ~> color)
          set canvas j color = store (addrOf canvas ! toIx (3 * fromIx ix + j + offset)) =<< cast color



writePixels :: KnownNat n => Canvas1D n -> Ref s1 (Array n (Struct RGB))
            -> Ivory ('Effects (Returns ()) r (Scope s2)) ()
writePixels canvas pixels =
    arrayMap $ \ix -> writePixel canvas ix (pixels ! ix)
