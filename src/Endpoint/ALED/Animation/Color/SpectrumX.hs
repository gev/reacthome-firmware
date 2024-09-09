module Endpoint.ALED.Animation.Color.SpectrumX where

import           Data.Record
import           Endpoint.ALED.Animation.CosT
import           Endpoint.ALED.Animation.Data
import           Ivory.Language


renderSpectrumX :: Uint16
                -> Sint32
                -> Sint32
                -> Record AnimationStruct
                -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSpectrumX segmentSize pixel subpixel animation = do
    time'  <- deref $ animation ~> time
    phase' <- deref $ animation ~> params ! toIx subpixel
    let x = time' + safeCast pixel / safeCast segmentSize :: IFloat
    let i = castDefault $ 255 * x + safeCast phase' :: Sint32
    cos'  <- deref $ addrOf cosT ! toIx i
    pure $ 255 * cos'
