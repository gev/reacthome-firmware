module Endpoint.ALED.Animation.Mask.Slide' where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib


renderSlide' :: Uint16
             -> Sint32
             -> Record AnimationStruct
             -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSlide' segmentSize pixel animation = do
    t' <- deref $ animation ~> time
    let x = castDefault $ (1 - t') * safeCast segmentSize
    safeCast <$> ifte (pixel ==? x)
                      (deref $ animation ~> params ! 1)
                      (deref $ animation ~> params ! 0)
