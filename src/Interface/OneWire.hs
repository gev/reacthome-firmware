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
    read  :: ow -> Ivory eff Uint8
    write :: ow -> Uint8 -> Ivory eff ()
