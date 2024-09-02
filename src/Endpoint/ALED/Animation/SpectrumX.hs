module Endpoint.ALED.Animation.SpectrumX where

import           Data.Record
import           Endpoint.ALED.Animation.Data
import           Endpoint.ALED.Animation.SinT
import           Ivory.Language


renderSpectrumX :: Uint8
                -> Sint32
                -> Sint32
                -> Record AnimationStruct
                -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSpectrumX segmentSize pixel subpixel animation = do
    time'  <- deref $ animation ~> time
    phase' <- deref $ animation ~> params ! toIx subpixel
    let x = time' + safeCast pixel / safeCast segmentSize :: IFloat
    let i = castDefault $ 255 * x + safeCast phase' :: Sint32
    sin'  <- deref $ addrOf sinT ! toIx i
    pure $ 255 * sin'
