module Endpoint.ALED.Animation.Mask.Eiffel where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.Data
import           Endpoint.ALED.Animation.SinT
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Random


renderEiffel :: Random Uint8
             -> Record AnimationStruct
             -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderEiffel random animation = do
    t'    <- deref $ animation ~> time
    let i  = castDefault $ t' * 255 :: Sint32
    p     <- (/ 4) <$> deref (addrOf sinT ! toIx i)
    p'    <- (/ 255) . safeCast <$> next random
    ifte (p' <? p)
         (pure 255)
         (pure 1)
