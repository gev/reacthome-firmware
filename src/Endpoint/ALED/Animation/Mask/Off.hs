module Endpoint.ALED.Animation.Mask.Off where

import Data.Record
import Endpoint.ALED
import Endpoint.ALED.Animation.Data
import Ivory.Language
import Ivory.Stdlib

renderOff ::
    Record AnimationStruct ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderOff = const $ pure 0
