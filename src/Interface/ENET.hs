{-# LANGUAGE RankNTypes #-}

module Interface.ENET where

import           Ivory.Language
import           Core.Handler


data HandleEnet t = HandleEnet
    { enet  :: e
    , handle :: forall eff. Ivory eff ()
    }