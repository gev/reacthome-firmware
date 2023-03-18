{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Core.Handler where

import           Control.Monad.Writer
import           Core.Context
import           Ivory.Language


class Handler h where
    addHandler :: h -> MonadWriter Context m => m ()
