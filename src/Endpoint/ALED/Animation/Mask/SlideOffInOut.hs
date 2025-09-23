module Endpoint.ALED.Animation.Mask.SlideOffInOut where

import Data.Record
import Endpoint.ALED
import Endpoint.ALED.Animation.Data
import Ivory.Language
import Ivory.Stdlib

renderSlideOffInOut ::
    IFloat ->
    Uint16 ->
    Sint32 ->
    Record AnimationStruct ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSlideOffInOut time segmentSize pixel animation = do
    inverse <- deref $ animation ~> inverseDirection
    ifte
        inverse
        ( do
            let x = castDefault $ (1 - time) * safeCast (segmentSize + segmentSize .& 1) / 2
            let x' = safeCast segmentSize - x - 1
            ifte
                (pixel >=? x .&& pixel <=? x')
                (pure 0)
                (pure 1)
        )
        ( do
            let x = castDefault $ time * safeCast (segmentSize + segmentSize .& 1) / 2
            let x' = safeCast segmentSize - x - 1
            ifte
                (pixel <=? x .|| pixel >=? x')
                (pure 0)
                (pure 1)
        )
