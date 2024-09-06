module Endpoint.ALED.Animation.Color.SpectrumT where

import           Data.Record
import           Endpoint.ALED.Animation.Data
import           Endpoint.ALED.Animation.SinT
import           Ivory.Language


renderSpectrumT :: Sint32
                -> Record AnimationStruct
                -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSpectrumT subpixel animation = do
    time'  <- deref $ animation ~> time
    phase' <- deref $ animation ~> params ! toIx subpixel
    let i   = castDefault $ (time' + safeCast phase' / 255) * 255 :: Sint32
    sin'  <- deref $ addrOf sinT ! toIx i
    pure $ 255 * sin'
