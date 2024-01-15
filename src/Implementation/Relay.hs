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
    , indicator :: Indicator
    }



relay :: Monad m => m Relays -> m Indicator -> m Relay
relay = liftA2 Relay



instance Controller Relay where
    handle Relay{..} buff size = do
        action <- unpack buff 0
        cond_ [ action ==? actionDo         ==> onDo       relays    buff size
              , action ==? actionGroup      ==> onGroup    relays    buff size
              , action ==? actionGetState   ==> onGetState relays
              , action ==? actionInitialize ==> onInit     relays    buff size
              , action ==? actionFindMe     ==> onFindMe   indicator buff size
              ]
