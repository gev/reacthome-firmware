{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Implementation.Smart.Bottom where

import           Core.Actions
import           Core.Controller
import           Feature.DInputs
import           Feature.DS18B20
import           Feature.Scd40
import           Feature.Sht21
import           Feature.Smart.Top
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib
import qualified Feature.Indicator as I



data Bottom n = Bottom
    { top     :: Top
    , dinputs :: DInputs n
    , indicator :: I.Indicator 16
    }



bottom1 :: (KnownNat n, Monad m)
        => m t
        -> (t -> m Top)
        -> (Bool -> t -> m (DInputs n))
        -> (t -> m DS18B20)
        -> (t -> m (I.Indicator 16)) 
        -> m (Bottom n)
bottom1 transport' top' dinputs' ds18b20 indicator' = do
    transport <- transport'
    ds18b20 transport
    dinputs <- dinputs' True transport
    top     <- top' transport
    indicator <- indicator' transport
    pure Bottom { top, dinputs, indicator }



bottom2 :: (KnownNat n, Monad m)
        => m t
        -> (t -> m Top)
        -> (Bool -> t
        -> m (DInputs n))
        -> (t -> m DS18B20)
        -> (t -> m SCD40)
        -> (t -> m (I.Indicator 16))
        -> m (Bottom n)
bottom2 transport top dinputs ds18b20 scd40 indicator'= do
    scd40 =<< transport
    bottom1 transport top dinputs ds18b20 indicator'



onGetState Bottom{..} buff size = do
    forceSyncDInputs dinputs
    forceSyncTop top



instance KnownNat n => Controller (Bottom n) where
    handle b@Bottom{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionSmartTop ==> onMessage  top       buff size
              , action ==? actionFindMe   ==> I.onFindMe indicator buff size
              , action ==? actionGetState ==> onGetState b         buff size
              ]
