{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Protocol.OneWire.Master.Search where

import           Control.Monad.State (MonadState)
import           Core.Context
import           Data.Value
import           Interface.OneWire
import           Ivory.Language



stateInit           = 0 :: Uint8
stateSearch         = 1 :: Uint8
stateReadBit        = 2 :: Uint8
stateWriteBit       = 3 :: Uint8
stateCheckDevices   = 4 :: Uint8
stateSuccess        = 5 :: Uint8
stateFailure        = 6 :: Uint8

data Search = Search
    { onewire :: OneWire
    , state   :: Value Uint8
    , time    :: Value Uint8

    }


mkSearch :: MonadState Context m => OneWire -> m Search
mkSearch onewire = do
    let name  = "one_wire_search"
    state    <- value (name <> "_state") stateInit
    time     <- value (name <> "_time" ) 0
    pure Search { onewire
                , state
                , time
                }
