module Endpoint.ALED.Animation.Color.Fade where

import           Data.Record
import           Endpoint.ALED                (ALED (ALED), brightness)
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib


renderFade :: Sint32
           -> IFloat
           -> Record AnimationStruct
           -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderFade subpixel value animation = do
    v  <- local $ ival value
    b  <- deref $ animation ~> params ! toIx subpixel
    dt <- deref $ animation ~> dt
    when (castDefault value /=? b) $ do
        inLoop' <- deref $ animation ~> inLoop
        ifte_ inLoop' (store v $ safeCast b) $ do
          time' <- deref $ animation ~> time
          let rest = 1 - time'
          ifte_ (rest >=? dt)
                (do
                  let delta = (safeCast b - value) * dt / rest
                  store v $ value + delta
                )
                (store v $ safeCast b)
    deref v
