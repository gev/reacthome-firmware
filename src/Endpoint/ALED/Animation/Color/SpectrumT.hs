module Endpoint.ALED.Animation.Color.SpectrumT where

import           Data.Record
import           Endpoint.ALED.Animation.CosT
import           Endpoint.ALED.Animation.Data
import           Ivory.Language


renderSpectrumT :: IFloat
                -> Sint32
                -> Record AnimationStruct
                -> IFloat
                -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSpectrumT time subpixel animation brightness = do
    phase' <- deref $ animation ~> params ! toIx (3 * subpixel)
    min    <- safeCast <$> deref (animation ~> params ! toIx (3 * subpixel + 1))
    max    <- safeCast <$> deref (animation ~> params ! toIx (3 * subpixel + 2))
    let i   = castDefault $ (time + safeCast phase' / 255) * 255 :: Sint32
    cos'   <- deref $ addrOf cosT ! toIx i
    pure $ brightness * (cos' * (max - min) + min)
