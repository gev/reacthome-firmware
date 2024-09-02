module Endpoint.ALED.Animation.RandomX where

import           Data.Record
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Random


renderRandomX :: IFloat
              -> Random Uint8
              -> Record AnimationStruct
              -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderRandomX value random animation = do
    dt' <- deref $ animation ~> dt
    i   <- next random
    ifte (safeCast i / 255  <=? dt' * safeCast fps)
         (safeCast <$> next random)
         (pure value)
