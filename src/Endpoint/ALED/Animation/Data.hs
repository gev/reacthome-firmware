{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

module Endpoint.ALED.Animation.Data where

import           Data.ByteString (split)
import           Ivory.Language


fps = 25 :: Uint32

type AnimationStruct = "animation_struct"

[ivory|
    struct animation_struct
    { kind           :: Uint8
    ; params         :: Array 8 (Stored Uint8)
    ; time           :: IFloat
    ; dt             :: IFloat
    ; inverse        :: IBool
    ; animationState :: IBool
    ; animationLoop  :: IBool
    }
|]
