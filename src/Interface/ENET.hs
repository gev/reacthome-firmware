{-# LANGUAGE RankNTypes #-}

module Interface.ENET where

import           Ivory.Language
import           Core.Handler


data HandleEnet e = HandleEnet
    { enet  :: e
    , handle :: forall eff. Ivory eff ()
    }