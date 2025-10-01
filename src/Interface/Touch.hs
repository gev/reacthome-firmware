module Interface.Touch where

import Ivory.Language

class Touch t where
    getDebug :: t -> Ivory eff IFloat
    getState :: t -> Ivory eff IBool
