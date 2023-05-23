{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Interface.EXTI where

import           Core.Handler
import           Ivory.Language


data HandleEXTI e = HandleEXTI
    { exti  :: e
    , handle :: forall eff. Ivory eff ()
    }


class Handler HandleEXTI e => EXTI e