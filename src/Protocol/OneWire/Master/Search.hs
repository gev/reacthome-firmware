{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Protocol.OneWire.Master.Search where

import           Control.Monad.State     (MonadState)
import           Core.Context
import           Protocol.OneWire.Master (OneWireMaster)



newtype Search = Search
    { onewire :: OneWireMaster
    }


mkSearch :: MonadState Context m => OneWireMaster -> m Search
mkSearch onewire =
    pure Search { onewire }
