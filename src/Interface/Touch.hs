{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Interface.Touch where

import           Core.Handler
import           Ivory.Language



data HandleTouch t = HandleTouch
    { touch  :: t
    , handle :: forall eff. Ivory eff ()
    }


class Handler HandleTouch t => Touch t where

    setModeInput  :: t -> Ivory eff ()
    setModeOutput :: t -> Ivory eff ()

    enable  :: t -> Ivory eff ()
    disable :: t -> Ivory eff ()