{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.Touch where

import           Core.Handler
import           Ivory.Language



-- data HandleTouch t = HandleTouch
--     { touch  :: t
--     , handle :: forall eff. Ivory eff ()
--     }


class Touch t where
    setModeInput  :: t -> Ivory eff ()
    setModeOutput :: t -> Ivory eff ()
    run           :: t -> Ivory eff () -> Ivory eff ()
    getTime       :: t -> Ivory eff IFloat
    getStateBtn   :: t -> Ivory eff IBool

