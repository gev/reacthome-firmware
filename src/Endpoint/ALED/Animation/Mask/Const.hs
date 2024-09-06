module Endpoint.ALED.Animation.Mask.Const where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib


renderConst :: Record AnimationStruct
            -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderConst = const $ pure 1
