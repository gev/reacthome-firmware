module Endpoint.ALED.Animation.Mask.SlideInOut where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib


renderSlideInOut :: IFloat
                 -> Uint16
                 -> Sint32
                 -> Record AnimationStruct
                 -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSlideInOut time segmentSize pixel animation = do
    d <- deref $ animation ~> params ! 2
    t <- local $ ival time
    inverseDirection' <- deref $ animation ~> inverseDirection
    when inverseDirection' $ store t (1 - time)
    t' <- deref t
    let x = castDefault $ t' * safeCast segmentSize / 2
    let x' = safeCast segmentSize - x - 1
    (/ (safeCast d + 1)) . safeCast
        <$> ifte (pixel ==? x .|| pixel ==? x')
                 (deref $ animation ~> params ! 1)
                 (deref $ animation ~> params ! 0)
