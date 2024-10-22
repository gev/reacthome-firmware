module Endpoint.ALED.Animation.Mask.Eiffel where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.CosT
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Random


renderEiffel :: IFloat
             -> Random Uint8
             -> Record AnimationStruct
             -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderEiffel time random animation = do
    let i  = castDefault $ time * 256 :: Sint32
    d     <- deref $ animation ~> params ! 2
    p     <- deref $ animation ~> params ! 3
    p'    <- (* safeCast p) <$> deref (addrOf cosT ! toIx i)
    v'    <- safeCast <$> next random
    (/ (safeCast d + 1)) . safeCast
        <$> ifte (v' <? p')
                 (deref $ animation ~> params ! 1)
                 (deref $ animation ~> params ! 0)
