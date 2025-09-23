
module Endpoint.ALED.Animation.Data where

import Data.ByteString (split)
import Ivory.Language

fps = 25 :: Uint32

type AnimationStruct = "animation_struct"

[ivory|
    struct animation_struct
    { kind:: Uint8
    ; params :: Array 12 (Stored Uint8)
    ; time :: IFloat
    ; timeEnd :: IFloat
    ; dt :: IFloat
    ; phase :: IFloat
    ; split :: IBool
    ; animationState :: IBool
    ; animationLoop :: IBool
    ; inLoop :: IBool
    ; startLoop :: IBool
    ; inverseDirection :: IBool
    }
|]
