{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Implementation.Dimmer where

import           Core.Actions
import           Core.Controller
import           Feature.Dimmers
import           Feature.Indicator (Indicator, onFindMe)
import           Ivory.Language
import           Ivory.Stdlib



data Dimmer = Dimmer
    { dimmers   :: Dimmers
    , indicator :: Indicator
    }



dimmer :: Monad m => m t -> (t -> m Dimmers) ->(t -> m Indicator) -> m Dimmer
dimmer transport' dimmers' indicator' = do
    transport <- transport'
    dimmers <- dimmers' transport
    indicator <- indicator' transport
    pure Dimmer { dimmers, indicator }



instance Controller Dimmer where
    handle Dimmer{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionDo         ==> onDo       dimmers buff size
              , action ==? actionDim        ==> onDim      dimmers buff size
              , action ==? actionInitialize ==> onInit     dimmers buff size
              , action ==? actionGetState   ==> onGetState dimmers
              , action ==? actionFindMe     ==> onFindMe   indicator buff size
              ]
