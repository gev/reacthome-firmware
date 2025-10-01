module Endpoint.ALED.Animation.Mask.Off where

import Data.Record
import Endpoint.ALED.Animation.Data
import Ivory.Language

renderOff ::
    Record AnimationStruct ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderOff = const $ pure 0
