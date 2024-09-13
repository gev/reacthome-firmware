module Endpoint.ALED.Animation.Mask.Slide'' where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib


renderSlide'' :: IFloat
              -> Uint16
              -> Sint32
              -> Record AnimationStruct
              -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSlide'' time segmentSize pixel animation = do
    let x = castDefault $ time * safeCast segmentSize
    let x' = safeCast segmentSize - x - 1
    safeCast <$> ifte (pixel ==? x .|| pixel ==? x')
                      (deref $ animation ~> params ! 1)
                      (deref $ animation ~> params ! 0)
