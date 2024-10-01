module Endpoint.ALED.Animation.Color.SpectrumT where

import           Data.Record
import           Endpoint.ALED.Animation.CosT
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib


renderSpectrumT :: IFloat
                -> Sint32
                -> Record AnimationStruct
                -> IFloat
                -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderSpectrumT time subpixel animation brightness = do
    phase' <- deref $ animation ~> params ! toIx (3 * subpixel)
    min <- safeCast <$> deref (animation ~> params ! toIx (3 * subpixel + 1))
    max <- safeCast <$> deref (animation ~> params ! toIx (3 * subpixel + 2))
    t <- local $ ival time
    inverseDirection' <- deref $ animation ~> inverseDirection
    when inverseDirection' $ store t (1 - time)
    t' <- deref t
    let i = castDefault t' * 255 + safeCast phase' :: Sint32
    cos' <- deref $ addrOf cosT ! toIx i
    pure $ brightness * (cos' * (max - min) + min)
