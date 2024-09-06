module Endpoint.ALED.Animation.Mask.Slide where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib


renderSlide :: Uint8
               -> Sint32
               -> Record AnimationStruct
               -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSlide segmentSize pixel animation = do
    t' <- deref $ animation ~> time
    let x = castDefault $ t' * safeCast segmentSize
    ifte (pixel ==? x)
         (pure 1)
         (pure 0)
