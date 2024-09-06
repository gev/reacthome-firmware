module Endpoint.ALED.Animation.Mask.RandomOn where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Random


renderRandomOn :: Sint32
               -> Random Uint8
               -> Record AnimationStruct
               -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderRandomOn pixel random animation = do
    let x   = pixel .% 64
    let px  = toIx $ x `iDiv` 8
    let b   = castDefault $ pixel .% 8
    let p   = animation ~> params ! px
    p'     <- deref p
    let isOff = (p' `iShiftR` b) .& 1
    ifte (isOff ==? 0)
         (do
            dt' <- deref $ animation ~> dt
            t'  <- deref $ animation ~> time
            t   <- next random
            ifte (safeCast t / 255 <? t'+ dt')
                 (do
                     store p $ p' .| (1 `iShiftL` b)
                     pure 255
                 )
                 (pure 0)
         )
         (pure 1)
