module Endpoint.ALED.Animation.Mask.Eiffel where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.CosT
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Random


renderEiffel :: Random Uint8
             -> Record AnimationStruct
             -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderEiffel random animation = do
    t'    <- deref $ animation ~> time
    let i  = castDefault $ t' * 256 :: Sint32
    p     <- safeCast <$> deref (animation ~> params ! 2)
    p'    <- (* p)    <$> deref (addrOf cosT ! toIx i)
    v'    <- safeCast <$> next random
    safeCast <$> ifte (v' <? p')
                      (deref $ animation ~> params ! 1)
                      (deref $ animation ~> params ! 0)
