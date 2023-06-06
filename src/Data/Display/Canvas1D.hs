{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Display.Canvas1D where

import           Control.Monad.Writer
import           Core.Context
import           Data.Color
import           Data.Display.FrameBuffer
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Proxy
import           Support.Cast


newtype Canvas1D (n :: Nat) b = Canvas1D {getBuffer :: b}

mkCanvas1D :: forall n b m. (KnownNat n, FrameBuffer b, MonadWriter Context m)
           => (Int -> m b) -> m (Canvas1D n b)
mkCanvas1D mkBuff = do
    let size = 3 * fromInteger (fromTypeNat (aNat :: NatType n))
    Canvas1D <$> mkBuff size



clearCanvas :: forall n b s. (KnownNat n, FrameBuffer b)
            => Canvas1D n b -> Ivory (ProcEffects s ()) ()
clearCanvas Canvas1D{..} =
    arrayMap $ \(ix :: Ix n) -> do
        let offset = 3 * fromIx ix
        clearByte getBuffer   offset
        clearByte getBuffer $ offset + 1
        clearByte getBuffer $ offset + 2



writePixel :: forall n b r s1 s2. (KnownNat n, FrameBuffer b)
            => Canvas1D n b -> Ix n -> Ref s1 (Struct RGB)
            -> Ivory ('Effects (Returns ()) r (Scope s2)) ()
writePixel Canvas1D{..} ix pixel = do
    let offset = 3 * fromIx ix
    writeByte getBuffer  offset      =<< cast g
    writeByte getBuffer (offset + 1) =<< cast r
    writeByte getBuffer (offset + 2) =<< cast b
    where cast c = castFloatToUint8 . (255 *) =<< deref (pixel ~> c)


writePixels :: forall n b r s1 s2. (KnownNat n, FrameBuffer b)
            => Canvas1D n b -> Ref s1 (Array n (Struct RGB))
            -> Ivory ('Effects (Returns ()) r (Scope s2)) ()
writePixels canvas pixels =
    arrayMap $ \ix -> writePixel canvas ix (pixels ! ix)
