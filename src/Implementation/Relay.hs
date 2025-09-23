{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Implementation.Relay where

import Control.Applicative
import Core.Actions
import Core.Controller
import Data.Serialize
import Feature.Indicator (Indicator, onFindMe)
import Feature.Relays (Relays, onDo, onGetState, onGroup, onInit)
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib

data Relay n = Relay
    { relays :: Relays n
    , indicator :: Indicator 20
    }

relay ::
    (Monad m) =>
    m t ->
    (t -> m (Relays n)) ->
    (t -> m (Indicator 20)) ->
    m (Relay n)
relay transport' relays' indicator' = do
    transport <- transport'
    relays <- relays' transport
    indicator <- indicator' transport
    pure Relay{relays, indicator}

instance (KnownNat n) => Controller (Relay n) where
    handle Relay{..} buff size = do
        action <- unpack buff 0
        cond_
            [ action ==? actionDo ==> onDo relays buff size
            , action ==? actionGroup ==> onGroup relays buff size
            , action ==? actionGetState ==> onGetState relays
            , action ==? actionInitialize ==> onInit relays buff size
            , action ==? actionFindMe ==> onFindMe indicator buff size
            ]
