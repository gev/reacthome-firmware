module Endpoint.ALED.Animation.Mask.FadeOff where

import Data.Record
import Endpoint.ALED
import Endpoint.ALED.Animation.Data
import Ivory.Language
import Ivory.Stdlib

renderFadeOff ::
    IFloat ->
    Record AnimationStruct ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderFadeOff time = const . pure $ 1 - time
