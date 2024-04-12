{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Smart.Top.Vibro where

import           Control.Monad.State (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Transport
import           Data.Buffer
import           Data.Value
import           Endpoint.DInputs    (DInputs (runDInputs), state)
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib



data Vibro = forall t. LazyTransport t => Vibro
    { dinputs   :: DInputs
    , volume    :: Value Uint8
    , transport :: t
    }



mkVibro :: (MonadState Context m, LazyTransport t)
          => DInputs -> t -> m Vibro
mkVibro dinputs transport = do
    volume <- value "vibro_volume" 1
    pure Vibro { dinputs, volume, transport}
