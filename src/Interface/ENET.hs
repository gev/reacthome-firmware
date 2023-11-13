{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Interface.ENET where

import           Ivory.Language
import           Core.Handler


data HandleEnet e = HandleEnet
    { enet  :: e
    , handle :: forall eff. Ivory eff ()
    }

class Handler HandleEnet e => Enet e where
    rxFrameSize :: e -> Ivory eff Uint32