module Endpoint.ALED.Animation.SpectrumX where

import           Data.Record
import           Endpoint.ALED.Animation.Data
import           Ivory.Language


renderSpectrumX :: Record AnimationStruct
                -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSpectrumX animation = pure 0
