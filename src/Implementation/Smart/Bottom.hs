{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Implementation.Smart.Bottom where

import           Control.Monad.Reader (MonadReader, asks)
import           Core.Actions
import           Core.Controller
import qualified Core.Domain          as D
import           Data.Value
import           Feature.ALED
import           Feature.DInputs
import           Feature.DS18B20
import           Feature.Scd40
import           Feature.Sht21
import           Feature.Smart.Top
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib



data Bottom n = Bottom
    { top        :: Top
    , dinputs    :: DInputs n
    , aled       :: ALED 10 100 2040
    , shouldInit :: Value IBool
    }



bottom1 :: (KnownNat n, MonadReader (D.Domain p c) m)
        => m t
        -> (t -> m Top)
        -> (Bool -> t -> m (DInputs n))
        -> (t -> m DS18B20)
        -> (t -> m (ALED 10 100 2040))
        -> m (Bottom n)
bottom1 transport' top' dinputs' ds18b20 aled' = do
    transport  <- transport'
    shouldInit <- asks D.shouldInit
    dinputs    <- dinputs' True transport
    top        <- top' transport
    aled       <- aled' transport
    ds18b20 transport
    pure Bottom { top, dinputs, aled, shouldInit }



bottom2 :: (KnownNat n, MonadReader (D.Domain p c) m)
        => m t
        -> (t -> m Top)
        -> (Bool -> t
        -> m (DInputs n))
        -> (t -> m DS18B20)
        -> (t -> m SCD40)
        -> (t -> m (ALED 10 100 2040))
        -> m (Bottom n)
bottom2 transport top dinputs ds18b20 scd40 aled'= do
    scd40 =<< transport
    bottom1 transport top dinputs ds18b20 aled'



onGetState Bottom{..} buff size = do
    forceSyncDInputs dinputs
    forceSyncTop top
    forceSyncAled aled



instance KnownNat n => Controller (Bottom n) where
    handle b@Bottom{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionSmartTop               ==> onMessage                top  buff size
              , action ==? actionFindMe                 ==> onFindMe                 top  buff size
              , action ==? actionGetState               ==> onGetState               b    buff size
              , action ==? actionInitialize             ==> onInitialize             aled buff size shouldInit
              , action ==? actionALedOn                 ==> onALedOn                 aled buff size
              , action ==? actionALedOff                ==> onALedOff                aled buff size
              , action ==? actionALedColorAnimationPlay ==> onALedColorAnimationPlay aled buff size
              , action ==? actionALedColorAnimationStop ==> onALedColorAnimationStop aled buff size
              , action ==? actionALedMaskAnimationPlay  ==> onALedMaskAnimationPlay  aled buff size
              , action ==? actionALedMaskAnimationStop  ==> onALedMaskAnimationStop  aled buff size
              , action ==? actionALedClip               ==> onALedClip               aled buff size
              , action ==? actionALedBrightness         ==> onALedBrightness         aled buff size
              , action ==? actionALedConfigGroup        ==> onALedConfigGroup        aled buff size
              ]
