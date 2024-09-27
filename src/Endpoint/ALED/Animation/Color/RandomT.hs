module Endpoint.ALED.Animation.Color.RandomT where

import           Data.Record
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Language.Float         (IFloat (IFloat))
import           Ivory.Stdlib
import           Util.Random


renderRandomT :: IFloat
              -> Sint32
              -> IFloat
              -> Random Uint8
              -> Record AnimationStruct
              -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderRandomT time subpixel value random animation = do
    let ix = toIx subpixel + 4
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
    startLoop' <- deref $ animation ~> startLoop
    when startLoop' $ do
        p <- deref $ animation ~> params ! toIx subpixel
        r <- next random
        let v = safeCast r * safeCast p / 255 :: IFloat
        store (animation ~> params ! ix) $ castDefault v
    deref v
