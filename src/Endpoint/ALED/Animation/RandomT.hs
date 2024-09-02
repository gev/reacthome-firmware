module Endpoint.ALED.Animation.RandomT where

import           Data.Record
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Random


renderRandomT :: Sint32
              -> IFloat
              -> Random Uint8
              -> Record AnimationStruct
              -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderRandomT subpixel value random animation = do
    v     <- local $ ival value
    b     <- deref $ animation ~> params ! toIx subpixel
    dt    <- deref $ animation ~> dt
    time' <- deref $ animation ~> time
    when (castDefault value /=? b) $ do
        time' <- deref $ animation ~> time
        let rest = 1 - time'
        ifte_ (rest >=? dt)
              (do
                let delta = (safeCast b - value) * dt / rest
                store v $ value + delta
              )
              (store v $ safeCast b)
    when (time' ==? 0) $ do
        store (animation ~> params ! toIx subpixel) =<< next random
    deref v
