module Endpoint.ALED.Animation.CosT where

import Ivory.Language

cosT :: ConstMemArea (Array 256 (Stored IFloat))
cosT =
    constArea "animation_cos_table" $
        iarray $
            ival . ifloat . f . fromIntegral
                <$> [0 .. 255]
  where
    f i = (1 - cos (pi * i / 128)) / 2
