module Endpoint.ALED.Animation.Mask.FadeOn where

import Data.Record
import Endpoint.ALED.Animation.Data
import Ivory.Language

renderFadeOn ::
    IFloat ->
    Record AnimationStruct ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderFadeOn time = const $ pure time
