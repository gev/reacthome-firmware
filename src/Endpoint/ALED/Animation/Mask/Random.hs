module Endpoint.ALED.Animation.Mask.Random where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Random


renderRandom :: Random Uint8
             -> Record AnimationStruct
             -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderRandom random animation = do
    t' <- deref $ animation ~> dt
    t  <- next random
    ifte (safeCast t / 255 <? t')
         (pure 255)
         (pure 1)
