{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Core.Handler where

import           Control.Monad.State
import           Core.Context
import           Ivory.Language


class Handler h t where
    addHandler :: h t -> MonadState Context m => m ()
