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
import           Data.Display.FrameBuffer
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Proxy
import           Support.Cast


newtype Canvas1D (n :: Nat) b = Canvas1D {getBuffer :: b}

mkCanvas1D :: forall n f t m. (KnownNat n, FrameBuffer f t, MonadState Context m)
           => (Int -> m (f t)) -> m (Canvas1D n (f t))
mkCanvas1D mkBuff = do
    let size = 3 * fromInteger (fromTypeNat (aNat :: NatType n))
    Canvas1D <$> mkBuff size



clearCanvas :: forall n f t s. (KnownNat n, FrameBuffer f t)
            => Canvas1D n (f t) -> Ivory (ProcEffects s ()) ()
clearCanvas Canvas1D{..} =
    arrayMap $ \(ix :: Ix n) -> do
        let offset = 3 * fromIx ix
        clearByte getBuffer   offset
        clearByte getBuffer $ offset + 1
        clearByte getBuffer $ offset + 2



writePixel :: forall n f t r s1 s2. (KnownNat n, FrameBuffer f t)
            => Canvas1D n (f t) -> Ix n -> Ref s1 (Struct RGB)
            -> Ivory ('Effects (Returns ()) r (Scope s2)) ()
writePixel Canvas1D{..} ix pixel = do
    let offset = 3 * fromIx ix
    writeByte getBuffer  offset      =<< cast g
    writeByte getBuffer (offset + 1) =<< cast r
    writeByte getBuffer (offset + 2) =<< cast b
    where cast c = castFloatToUint8 . (255 *) =<< deref (pixel ~> c)


writePixels :: forall n f t r s1 s2. (KnownNat n, FrameBuffer f t)
            => Canvas1D n (f t) -> Ref s1 (Array n (Struct RGB))
            -> Ivory ('Effects (Returns ()) r (Scope s2)) ()
writePixels canvas pixels =
    arrayMap $ \ix -> writePixel canvas ix (pixels ! ix)
