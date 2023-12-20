{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.ENET where

import           Core.Handler
import           Ivory.Language


data HandleEnet e = HandleEnet
    { enet   :: e
    , handle :: forall eff. Ivory eff ()
    }

class Handler HandleEnet e => Enet e where
    initEth     :: e -> Ivory eff IBool
    rxFrameSize :: e -> Ivory eff Uint32
