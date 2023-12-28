{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
module Feature.Server where

import           Control.Monad           (zipWithM_)
import           Control.Monad.Reader    (MonadReader, asks)
import           Control.Monad.State     (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Controller
import qualified Core.Domain             as D
import           Core.Feature
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Serialize
import           Data.Value
import           Endpoint.Dimmers        (Dimmers (..), dimmers, initialize,
                                          syncDimmerGroup)
import qualified Endpoint.DInputs        as DI
import           Feature.Dimmer.DC       (DimmerDC (getDimmers, n),
                                          forceSyncDimmerDC, mkDimmerDC, onDim,
                                          onDo)
import           Feature.DInputs         (DInputs (getDInputs),
                                          forceSyncDInputs, manageDInputs,
                                          mkDInputs, syncDInputs)
import           Feature.RS485.RBUS      (configureMode, forceSyncRBUS,
                                          forceSyncRBUS', mkRBUS, setMode,
                                          transmitRB485, transmitRBUS)
import           Feature.RS485.RBUS.Data (RBUS (RBUS, baudrate, lineControl, mode))
import           GHC.TypeNats
import           Interface.GPIO.Input
import           Interface.GPIO.Port
import           Interface.PWM           (PWM)
import           Interface.RS485
import           Ivory.Language
import           Ivory.Stdlib


data Server = Server
    { rbus       :: [RBUS]
    , dimmer     :: DimmerDC
    , dinputs    :: DInputs
    , shouldInit :: Value IBool
    }


server :: ( MonadState Context m
          , MonadReader (D.Domain p t) m
          , LazyTransport t, Transport t, PWM o, Input i, Pull p u
          ) => [m RS485] -> [p -> Uint32 -> Uint32 -> m o] -> [p -> u -> m i] -> m Feature
server rs485 pwms inputs = do
    rbus       <- mkRBUS     rs485
    dimmer     <- mkDimmerDC pwms
    dinputs    <- mkDInputs  inputs True
    shouldInit <- asks D.shouldInit

    addTask  $ delay 10 "dinputs_manage" $ manageDInputs dinputs
    addTask  $ yeld     "dinputs_sync"   $ syncDInputs   dinputs

    addSync "dinputs" $ forceSyncDInputs  dinputs
    addSync "dimmers" $ forceSyncDimmerDC dimmer

    pure $ Feature Server { rbus, dimmer, dinputs, shouldInit }


instance Controller Server where
    handle s@Server{..} buff size = do
        action <- deref $ buff ! 0
        pure [ action ==? actionDo            ==> onDo          dimmer buff size
             , action ==? actionDim           ==> onDim         dimmer buff size
             , action ==? actionRs485Mode     ==> setMode       rbus   buff size
             , action ==? actionRbusTransmit  ==> transmitRBUS  rbus   buff size
             , action ==? actionRs485Transmit ==> transmitRB485 rbus   buff size
             , action ==? actionInitialize    ==> onInit        s      buff size
             , action ==? actionGetState      ==> onGetState    s
             ]


onInit :: KnownNat n => Server -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onInit Server{..} buff size =
    when (size ==? 25 + n dimmer * 3) $ do

        let run r@RBUS{..} offset = do
                store mode        =<< unpack   buff  offset
                store baudrate    =<< unpackLE buff (offset + 1)
                store lineControl =<< unpack   buff (offset + 5)
                configureMode r
        zipWithM_ run rbus $ fromIntegral <$> [1, 7..]

        offset <- local $ ival 25
        runDimmers (getDimmers dimmer) $ \ds -> do
            arrayMap $ \ix -> do
                offset' <- deref offset
                let d = addrOf ds ! ix
                group    <- unpack buff  offset'
                mode     <- unpack buff (offset' + 1)
                value    <- unpack buff (offset' + 2) :: Ivory eff Uint8
                initialize d group mode (safeCast value / 255) 0
                syncDimmerGroup ds d ix
                store offset $ offset' + 3

        store shouldInit false



onGetState Server{..} = do
    forceSyncDInputs dinputs
    forceSyncDimmerDC dimmer
    forceSyncRBUS' rbus
