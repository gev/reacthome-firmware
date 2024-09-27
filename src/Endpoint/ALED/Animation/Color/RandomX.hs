module Endpoint.ALED.Animation.Color.RandomX where

import           Data.Record
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Random


renderRandomX :: Sint32
              -> IFloat
              -> Random Uint8
              -> Record AnimationStruct
              -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderRandomX subpixel value random animation = do
    p   <- deref $ animation ~> params ! toIx subpixel
    dt' <- deref $ animation ~> dt
    i   <- next random
    ifte (safeCast i / 255 <=? dt' * safeCast fps)
         ((/ 255) . (* safeCast p) . safeCast <$> next random)
         (pure value)
