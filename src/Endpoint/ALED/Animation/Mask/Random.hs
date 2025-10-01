module Endpoint.ALED.Animation.Mask.Random where

import Data.Record
import Endpoint.ALED.Animation.Data
import Ivory.Language
import Ivory.Stdlib
import Util.Random

renderRandom ::
    Random Uint8 ->
    Record AnimationStruct ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderRandom random animation = do
    d <- deref $ animation ~> params ! 2
    p <- deref $ animation ~> params ! 3
    p' <- next random
    (/ (safeCast d + 1)) . safeCast
        <$> ifte
            (p' <? p)
            do deref $ animation ~> params ! 1
            do deref $ animation ~> params ! 0
