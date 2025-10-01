module Endpoint.ALED.Animation.Mask.SlideOnInOut where

import Data.Record
import Endpoint.ALED.Animation.Data
import Ivory.Language
import Ivory.Stdlib

renderSlideOnInOut ::
    IFloat ->
    Uint16 ->
    Sint32 ->
    Record AnimationStruct ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSlideOnInOut time segmentSize pixel animation = do
    inverse <- deref $ animation ~> inverseDirection
    ifte
        inverse
        do
            let x = castDefault $ (1 - time) * safeCast (segmentSize + segmentSize .& 1) / 2
            let x' = safeCast segmentSize - x - 1
            ifte
                (pixel >=? x .&& pixel <=? x')
                do pure 1
                do pure 0
        do
            let x = castDefault $ time * safeCast (segmentSize + segmentSize .& 1) / 2
            let x' = safeCast segmentSize - x - 1
            ifte
                (pixel <=? x .|| pixel >=? x')
                do pure 1
                do pure 0
