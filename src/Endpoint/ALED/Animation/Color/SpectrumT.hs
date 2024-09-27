module Endpoint.ALED.Animation.Color.SpectrumT where

import           Data.Record
import           Endpoint.ALED.Animation.CosT
import           Endpoint.ALED.Animation.Data
import           Ivory.Language


renderSpectrumT :: IFloat
                -> Sint32
                -> Record AnimationStruct
                -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSpectrumT time subpixel animation = do
    phase' <- deref $ animation ~> params ! toIx (2 * subpixel)
    k'     <- deref $ animation ~> params ! toIx (2 * subpixel + 1)
    let i   = castDefault $ (time + safeCast phase' / 255) * 255 :: Sint32
    cos'   <- deref $ addrOf cosT ! toIx i
    pure $ safeCast k' * cos'
