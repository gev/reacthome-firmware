module Endpoint.ALED.Animation.Mask.SlideOff where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib


renderSlideOff :: Uint8
               -> Sint32
               -> Record AnimationStruct
               -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSlideOff segmentSize pixel animation = do
    t' <- deref $ animation ~> time
    let x = castDefault $ t' * safeCast segmentSize
    ifte (pixel <=? x)
         (pure 0)
         (pure 1)
