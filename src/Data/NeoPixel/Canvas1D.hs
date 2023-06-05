{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.NeoPixel.Canvas1D where

import           Control.Monad.Writer
import           Core.Context
import           Data.Color
import           Data.NeoPixel.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Proxy


newtype Canvas1D (n :: Nat) b = Canvas1D {getBuffer :: b}

mkCanvas1D :: forall n b m. (KnownNat n, NeoPixelBuffer b, MonadWriter Context m)
           => (Int -> m b) -> m (Canvas1D n b)
mkCanvas1D mkBuff = do
    let size = 3 * fromInteger (fromTypeNat (aNat :: NatType n))
    Canvas1D <$> mkBuff size



clearCanvas :: forall n b s. (KnownNat n, NeoPixelBuffer b)
            => Canvas1D n b -> Ivory (ProcEffects s ()) ()
clearCanvas Canvas1D{..} =
    arrayMap $ \(ix :: Ix n) -> do
        let offset = 3 * fromIx ix
        clearByte getBuffer offset
        clearByte getBuffer $ offset + 1
        clearByte getBuffer $ offset + 2



writePixel :: forall n b r s. (KnownNat n, NeoPixelBuffer b)
            => Canvas1D n b -> Ix n -> RGB
            -> Ivory ('Effects (Returns ()) r (Scope s)) ()
writePixel Canvas1D{..} ix RGB{..} = do
    let offset = 3 * fromIx ix
    writeByte getBuffer offset g
    writeByte getBuffer (offset + 1) r
    writeByte getBuffer (offset + 2) b
