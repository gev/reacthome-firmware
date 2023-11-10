{-# LANGUAGE RankNTypes #-}

module Interface.ENET where

import           Ivory.Language
import           Core.Handler


data HandleTimer t = HandleTimer
    { enet  :: t
    , handle :: forall eff. Ivory eff ()
    }