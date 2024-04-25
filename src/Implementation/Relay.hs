{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Implementation.Relay where

import           Control.Applicative
import           Core.Actions
import           Core.Controller
import           Data.Serialize
import           Feature.Indicator   (Indicator, onFindMe)
import           Feature.Relays      (Relays, onDo, onGetState, onGroup, onInit)
import           Ivory.Language
import           Ivory.Stdlib



data Relay = Relay
    { relays    :: Relays
    , indicator :: Indicator 20
    }



relay :: Monad m => m t -> (t -> m Relays) -> (t -> m (Indicator 20)) -> m Relay
relay transport' relays' indicator' = do
    transport <- transport'
    relays    <- relays' transport
    indicator <- indicator' transport
    pure Relay {relays, indicator}



instance Controller Relay where
    handle Relay{..} buff size = do
        action <- unpack buff 0
        cond_ [ action ==? actionDo         ==> onDo       relays    buff size
              , action ==? actionGroup      ==> onGroup    relays    buff size
              , action ==? actionGetState   ==> onGetState relays
              , action ==? actionInitialize ==> onInit     relays    buff size
              , action ==? actionFindMe     ==> onFindMe   indicator buff size
              ]
