module Endpoint.ALED.Animation.Color.SpectrumX where

import           Data.Record
import           Endpoint.ALED.Animation.CosT
import           Endpoint.ALED.Animation.Data
import           Ivory.Language


renderSpectrumX :: IFloat
                -> Uint16
                -> Sint32
                -> Sint32
                -> Record AnimationStruct
                -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSpectrumX time segmentSize pixel subpixel animation = do
    phase' <- deref $ animation ~> params ! toIx (2 * subpixel)
    k'     <- deref $ animation ~> params ! toIx (2 * subpixel + 1)
    let x   = time + safeCast pixel / safeCast segmentSize :: IFloat
    let i   = castDefault $ 255 * x + safeCast phase' :: Sint32
    cos'   <- deref $ addrOf cosT ! toIx i
    pure $ safeCast k' * cos'
