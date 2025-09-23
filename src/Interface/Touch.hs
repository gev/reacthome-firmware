{-# LANGUAGE RankNTypes #-}

module Interface.Touch where

import Core.Handler
import Ivory.Language

class Touch t where
    getDebug :: t -> Ivory eff IFloat
    getState :: t -> Ivory eff IBool
