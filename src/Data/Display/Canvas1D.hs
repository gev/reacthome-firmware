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



data Canvas1D n = Canvas1D
    { canvas    :: Values n Uint8
    , offset    :: Sint32
    }



mkCanvas1D :: Values n Uint8 -> Sint32  -> Canvas1D n
mkCanvas1D = Canvas1D



clearCanvas :: forall n s. KnownNat n => Canvas1D n -> Ivory (ProcEffects s ()) ()
clearCanvas Canvas1D{..} = do
    let size = fromInteger $ fromTypeNat (aNat :: NatType n) :: Ix n
    upTo (toIx offset) (toIx $ offset + fromIx size) $ \ix ->
        store (canvas ! ix) 0



writePixel :: KnownNat n => Canvas1D n -> Ix n -> Ref s1 (Struct RGB)
           -> Ivory ('Effects (Returns ()) r (Scope s2)) ()
writePixel Canvas1D{..} ix pixel = 
    set 0 g >> set 1 r >> set 2 b
    where cast color = castFloatToUint8 . (255 *) =<< deref (pixel ~> color)
          set j color = store (canvas ! toIx (3 * fromIx ix + j + offset)) =<< cast color



writePixels :: (KnownNat n1, KnownNat n2) => Canvas1D n1 -> Ref s1 (Array n2 (Struct RGB))
            -> Ivory ('Effects (Returns ()) r (Scope s2)) ()
writePixels canvas pixels =
    arrayMap $ \ix -> writePixel canvas ix (pixels ! toIx ix)
