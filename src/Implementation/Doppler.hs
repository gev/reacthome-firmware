{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Implementation.Doppler where

import           Control.Monad.Reader (MonadReader, asks)
import           Core.Actions
import           Core.Controller
import qualified Core.Domain          as D
import           Data.Value
import           Feature.ALED
import           Feature.DInputs
import           Feature.Dopplers
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib

data Doppler nd ni = Doppler
    { dopplers   :: Dopplers nd
    , dinputs    :: DInputs  ni
    , aled       :: ALED     10 100 2400
    , shouldInit :: Value IBool
    }

doppler :: MonadReader (D.Domain p c) m
        => m t
        -> (t -> m (Dopplers nd))
        -> (Bool -> t -> m (DInputs ni))
        -> (t -> m (ALED 10 100 2400))
        -> m (Doppler nd ni)
doppler transport' dopplers' dinputs' aled' = do
    transport  <- transport'
    shouldInit <- asks D.shouldInit
    dopplers   <- dopplers' transport
    dinputs    <- dinputs' True transport
    aled       <- aled' transport
    pure Doppler { dopplers, dinputs, aled, shouldInit}

onGetState Doppler{..} buff size = do
    forceSyncDInputs dinputs
    forceSyncAled aled

instance KnownNat ni => Controller (Doppler nd ni) where

    handle d@Doppler{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionGetState               ==> onGetState               d    buff size
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
