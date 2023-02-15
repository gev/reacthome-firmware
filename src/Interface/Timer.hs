{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.Timer where

import           Core.Include
import           Core.Initialize
import           Ivory.Language
import           Ivory.Stdlib


data HandleTimer t = HandleTimer
    { timer  :: t
    , handle :: forall eff. Ivory eff ()
    }


class (Include (HandleTimer t), Initialize (HandleTimer t)) => Timer t
