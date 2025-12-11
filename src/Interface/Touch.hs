module Interface.Touch where

import Ivory.Language

data Material = Material
    { maxDiff :: IFloat
    , thresholdUp :: IFloat
    , thresholdDown :: IFloat
    }

class Touch t where
    getDebug :: t -> Ivory eff IFloat
    getState :: t -> Ivory eff IBool