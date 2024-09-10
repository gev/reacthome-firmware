module Endpoint.ALED.Animation.Mask.SlideOnIn where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib


renderSlideOnIn :: IFloat 
                -> Uint16
                -> Sint32
                -> Record AnimationStruct
                -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSlideOnIn time segmentSize pixel animation = do
    let x = castDefault $ time * safeCast (segmentSize + segmentSize .& 1) / 2
    let x' = safeCast segmentSize - x - 1
    ifte (pixel <=? x .|| pixel >=? x')
         (pure 1)
         (pure 0)