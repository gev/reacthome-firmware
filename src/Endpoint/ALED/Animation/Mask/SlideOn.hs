module Endpoint.ALED.Animation.Mask.SlideOn where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib


renderSlideOn :: Uint16
              -> Sint32
              -> IFloat
              -> Record AnimationStruct
              -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSlideOn segmentSize pixel t animation = do
    let x = castDefault $ t * safeCast segmentSize
    ifte (pixel <=? x)
         (pure 1)
         (pure 0)
