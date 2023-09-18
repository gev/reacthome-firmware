{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.OneWire where

import           Core.Handler
import           Ivory.Language

data HandleOneWire ow = HandleOneWire
    { onewire :: ow
    , onRead  :: forall eff. Uint8 -> Ivory eff ()
    }

class Handler HandleOneWire ow => OneWire ow where
    reset :: ow -> Ivory eff ()
    read  :: ow -> Ivory eff ()
    write :: ow -> Uint8 -> Ivory eff ()
