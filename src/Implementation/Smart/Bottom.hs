{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Implementation.Smart.Bottom where

import           Core.Actions
import           Core.Controller
import           Feature.ALED
import           Feature.DInputs
import           Feature.DS18B20
import           Feature.Scd40
import           Feature.Sht21
import           Feature.Smart.Top (Top, forceSyncTop, onMessage)
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib



data Bottom n = Bottom
    { top     :: Top
    , dinputs :: DInputs n
    , aled    :: ALED 10 100 3000
    }



bottom1 :: (KnownNat n, Monad m)
        => m t
        -> (t -> m Top)
        -> (Bool -> t -> m (DInputs n))
        -> (t -> m DS18B20)
        -> (t -> m (ALED 10 100 3000))
        -> m (Bottom n)
bottom1 transport' top' dinputs' ds18b20 aled' = do
    transport <- transport'
    ds18b20 transport
    dinputs <- dinputs' True transport
    top     <- top' transport
    aled    <- aled' transport
    pure Bottom { top, dinputs, aled }



bottom2 :: (KnownNat n, Monad m)
        => m t
        -> (t -> m Top)
        -> (Bool -> t
        -> m (DInputs n))
        -> (t -> m DS18B20)
        -> (t -> m SCD40)
        -> (t -> m (ALED 10 100 3000))
        -> m (Bottom n)
bottom2 transport top dinputs ds18b20 scd40 aled'= do
    scd40 =<< transport
    bottom1 transport top dinputs ds18b20 aled'



onGetState Bottom{..} buff size = do
    forceSyncDInputs dinputs
    forceSyncTop top



instance KnownNat n => Controller (Bottom n) where
    handle b@Bottom{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionSmartTop ==> onMessage  top buff size
              , action ==? actionGetState ==> onGetState b   buff size
              ]
