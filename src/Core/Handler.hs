{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Core.Handler where

import           Control.Monad.Writer
import           Core.Context
import           Ivory.Language


class Handler h t where
    addHandler :: h t -> MonadWriter Context m => m ()
