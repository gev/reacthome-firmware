module Endpoint.ALED.Animation.Color.RandomT where

import           Data.Record
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Random


renderRandomT :: IFloat
              -> Sint32
              -> IFloat
              -> Random Uint8
              -> Record AnimationStruct
              -> IFloat
              -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderRandomT time subpixel value random animation brightness = do
    let ix = toIx subpixel + 8
    startLoop' <- deref $ animation ~> startLoop
    when startLoop' $ do
        min <- safeCast <$> deref (animation ~> params ! toIx (2 * subpixel))
        max <- safeCast <$> deref (animation ~> params ! toIx (2 * subpixel + 1))
        r   <- safeCast <$> next random
        let v' = brightness * (r * (max - min) / 255 + min)
        store (animation ~> params ! ix) $ castDefault v'
    v  <- local $ ival value
    b  <- deref $ animation ~> params ! ix
    dt <- deref $ animation ~> dt
    when (castDefault value /=? b) $ do
        let rest = 1 - time
        ifte_ (rest >=? dt)
              (do
                let delta = (safeCast b - value) * dt / rest
                store v $ value + delta
              )
              (store v $ safeCast b)
    deref v
