{-# LANGUAGE DataKinds #-}

module Endpoint.ALED.Animation.SinT where

import           Ivory.Language



sinT :: ConstMemArea (Array 256 (Stored IFloat))
sinT = constArea "animation_sin_table" $ iarray $ ival . ifloat . f . fromIntegral <$> [0..255]
    where f i = (1 + sin (pi * i / 128)) / 2
