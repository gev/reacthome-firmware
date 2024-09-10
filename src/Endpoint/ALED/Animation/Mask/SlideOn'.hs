module Endpoint.ALED.Animation.Mask.SlideOn' where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib


renderSlideOn' :: IFloat
               -> Uint16
               -> Sint32
               -> Record AnimationStruct
               -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSlideOn' time segmentSize pixel animation = do
    let x = castDefault $ (1 - time) * safeCast segmentSize
    ifte (pixel >=? x)
         (pure 1)
         (pure 0)
