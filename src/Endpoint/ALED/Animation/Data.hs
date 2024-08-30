{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

module Endpoint.ALED.Animation.Data where

import           Ivory.Language


type AnimationStruct = "animation_struct"

[ivory|
    struct animation_struct
    { kind           :: Uint8
    ; params         :: Array 8 (Stored Uint8)
    ; time           :: IFloat
    ; dt             :: IFloat
    ; animationState :: IBool
    ; animationLoop  :: IBool
    }
|]
