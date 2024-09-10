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
              -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderRandomT time subpixel value random animation = do
    v  <- local $ ival value
    b  <- deref $ animation ~> params ! toIx subpixel
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
        store (animation ~> params ! toIx subpixel) =<< next random
    deref v
