module Endpoint.ALED.Animation.Mask.On where

import Data.Record
import Endpoint.ALED
import Endpoint.ALED.Animation.Data
import Ivory.Language
import Ivory.Stdlib

renderOn ::
    Record AnimationStruct ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderOn = const $ pure 1
