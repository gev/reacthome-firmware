{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.Timer where

import           Interface
import           Ivory.Language
import           Ivory.Stdlib


data HandleTimer t = HandleTimer
  { timer  :: t
  , handle :: forall eff. Ivory eff ()
  }


class (Interface (HandleTimer t)) => Timer t
