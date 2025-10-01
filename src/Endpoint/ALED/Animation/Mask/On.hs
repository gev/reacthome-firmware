module Endpoint.ALED.Animation.Mask.On where

import Data.Record
import Endpoint.ALED.Animation.Data
import Ivory.Language

renderOn ::
    Record AnimationStruct ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderOn = const $ pure 1
