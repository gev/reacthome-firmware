module Endpoint.ALED.Animation.Mask.SlideOff where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib


renderSlideOff :: IFloat
               -> Uint16
               -> Sint32
               -> Record AnimationStruct
               -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSlideOff time segmentSize pixel animation = do
    inverse <- deref $ animation ~> inverseDirection
    ifte inverse
        (do
            let x = castDefault $ (1 - time) * safeCast segmentSize
            ifte (pixel >=? x)
                 (pure 0)
                 (pure 1)
        )
        (do
            let x = castDefault $ time * safeCast segmentSize
            ifte (pixel <=? x)
                 (pure 0)
                 (pure 1)

        )
