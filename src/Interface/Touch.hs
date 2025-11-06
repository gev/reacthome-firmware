module Interface.Touch where

import Ivory.Language

data Material = Material
    { maxMoment :: Uint32
    , maxDiff :: IFloat
    , thresholdUp :: IFloat
    , thresholdDown :: IFloat
    }

class Touch t where
    getDebug :: t -> Ivory eff IFloat
    getState :: t -> Ivory eff IBool
