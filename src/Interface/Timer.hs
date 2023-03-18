{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.Timer where

import           Core.Context
import           Core.Handler
import           Ivory.Language
import           Ivory.Stdlib


data HandleTimer t = HandleTimer
    { timer  :: t
    , handle :: forall eff. Ivory eff ()
    }


class Handler (HandleTimer t) => Timer t
