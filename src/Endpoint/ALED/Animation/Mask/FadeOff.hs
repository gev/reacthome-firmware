module Endpoint.ALED.Animation.Mask.FadeOff where

import Data.Record
import Endpoint.ALED.Animation.Data
import Ivory.Language

renderFadeOff ::
    IFloat ->
    Record AnimationStruct ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderFadeOff time = const . pure $ 1 - time
