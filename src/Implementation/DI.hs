{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Implementation.DI where

import           Control.Monad
import           Core.Actions
import           Core.Controller
import           Feature.ALED
import           Feature.DInputs (DInputs, forceSyncDInputs)
import           Feature.DS18B20
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib




data DI n = DI
    { dinputs :: DInputs n
    , aled    :: ALED 10 100 2040
    }



di :: Monad m => m t -> (Bool -> t -> m (DInputs n)) -> (t -> m DS18B20) -> (t -> m (ALED 10 100 2040)) -> m (DI n)
di transport' dinputs' ds18b20 aled' = do
    transport <- transport'
    ds18b20 transport
    dinputs <- dinputs' True transport
    aled    <- aled' transport
    pure DI { dinputs, aled }

onGetState DI{..} buff size = do
    forceSyncDInputs dinputs
    forceSyncAled aled

instance KnownNat n => Controller (DI n) where
    handle d@DI{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionGetState               ==> onGetState               d    buff size
              , action ==? actionInitialize             ==> onInitialize             aled buff size
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
