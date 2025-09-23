{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Data.Display.Canvas1D where

import Control.Monad.State
import Core.Context
import Data.Color
import Data.Value
import GHC.TypeNats
import Ivory.Language
import Ivory.Language.Proxy
import Ivory.Stdlib
import Support.Cast

type Canvas1DSize n = 3 * n

newtype Canvas1D n = Canvas1D
    { canvas :: Values (Canvas1DSize n) Uint8
    }

mkCanvas1D :: Values (Canvas1DSize n) Uint8 -> Canvas1D n
mkCanvas1D = Canvas1D

clearCanvas ::
    (KnownNat (Canvas1DSize n)) =>
    Canvas1D n ->
    Ivory (ProcEffects s ()) ()
clearCanvas Canvas1D{..} =
    arrayMap $ \ix -> store (canvas ! ix) 0

writePixel ::
    (KnownNat n, KnownNat (Canvas1DSize n)) =>
    Canvas1D n ->
    Ix n ->
    Ref s1 (Struct RGB) ->
    Ivory ('Effects (Returns ()) r (Scope s2)) IBool
writePixel Canvas1D{..} ix pixel = do
    isUpdatedR <- set 0 g
    isUpdatedG <- set 1 r
    isUpdatedB <- set 2 b
    pure $ isUpdatedR .|| isUpdatedG .|| isUpdatedB
  where
    cast color = castFloatToUint8 . (255 *) =<< deref (pixel ~> color)
    set j color = do
        let src = canvas ! toIx (3 * fromIx ix + j)
        src' <- deref src
        dst' <- cast color
        store src dst'
        pure $ src' /=? dst'

writePixels ::
    (KnownNat n, KnownNat (Canvas1DSize n)) =>
    Canvas1D n ->
    Ref s1 (Array n (Struct RGB)) ->
    Ivory ('Effects (Returns ()) r (Scope s2)) IBool
writePixels canvas pixels = do
    shouldUpdate <- local $ ival false
    arrayMap $ \ix -> do
        isUpdated <- writePixel canvas ix (pixels ! toIx ix)
        when isUpdated $ store shouldUpdate true
    deref shouldUpdate
