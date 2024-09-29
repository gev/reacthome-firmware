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
                -> IFloat
                -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSpectrumX time segmentSize pixel subpixel animation brightness = do
    phase' <- deref $ animation ~> params ! toIx (3 * subpixel)
    min    <- safeCast <$> deref (animation ~> params ! toIx (3 * subpixel + 1))
    max    <- safeCast <$> deref (animation ~> params ! toIx (3 * subpixel + 2))
    let x   = time + safeCast pixel / safeCast segmentSize
    let i   = castDefault $ 255 * x + safeCast phase' :: Sint32
    cos'   <- deref $ addrOf cosT ! toIx i
    pure $ brightness * (cos' * (max - min) + min)
