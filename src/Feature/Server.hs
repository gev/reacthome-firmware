{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
module Feature.Server where

import           Control.Monad           (zipWithM_)
import           Control.Monad.Reader    (MonadReader)
import           Control.Monad.State     (MonadState)
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Serialize
import           Endpoint.Dimmers        (Dimmers (..), initialize,
                                          syncDimmerGroup)
import qualified Endpoint.DInputs        as DI
import           Feature.Dimmer.DC       (DimmerDC (..), mkDimmerDC, onDim,
                                          onDo)
import           Feature.DInputs         (DInputs (getDInputs), manageDInputs,
                                          mkDInputs, syncDInputs)
import           Feature.RS485.RBUS      (configureMode, mkRBUS, setMode,
                                          transmitRB485, transmitRBUS)
import           Feature.RS485.RBUS.Data
import           GHC.TypeNats
import           Interface.GPIO.Input
import           Interface.PWM           (PWM)
import           Interface.RS485
import           Ivory.Language
import           Ivory.Stdlib


data Server = Server
    { rbus    :: [RBUS]
    , dimmer  :: DimmerDC
    , dinputs :: DInputs
    }


server :: ( MonadState Context m
          , MonadReader (Domain p t) m
          , LazyTransport t, Transport t, PWM o, Input i
          ) => [m RS485] -> [p -> Uint32 -> Uint32 -> m o] -> [p -> m i] -> m Feature
server rs485 pwms inputs = do
    rbus    <- mkRBUS     rs485
    dimmer  <- mkDimmerDC pwms
    dinputs <- mkDInputs  inputs

    addTask  $ delay 10 "dinputs_manage" $ manageDInputs dinputs
    addTask  $ yeld     "dinputs_sync"   $ syncDInputs   dinputs

    addSync "dinputs" $ DI.runDInputs (getDInputs dinputs) $
        \dis -> arrayMap $ \ix -> store (addrOf dis ! ix ~> DI.synced) false

    pure $ Feature Server { rbus, dimmer, dinputs }


instance Controller Server where
    handle Server{..} buff size = do
        action <- deref $ buff ! 0
        pure [ action ==? 0xa0 ==> setMode       rbus buff size
             , action ==? 0xa1 ==> transmitRBUS  rbus buff size
             , action ==? 0xa2 ==> transmitRB485 rbus buff size
             , action ==? 0x00 ==> onDo  dimmer buff size
             , action ==? 0xd0 ==> onDim dimmer buff size
             , action ==? 0xf2 ==> onInit rbus dimmer buff size
             ]


onInit :: KnownNat n => [RBUS] -> DimmerDC -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s ()) ()
onInit rbus DimmerDC{..} buff size =
    when (size ==? 25 + n * 3) $ do

        let run r@RBUS{..} offset = do
                store mode        =<< unpack   buff  offset
                store baudrate    =<< unpackLE buff (offset + 1)
                store lineControl =<< unpack   buff (offset + 5)
                configureMode r
        zipWithM_ run rbus $ fromIntegral <$> [1, 7..]

        offset <- local $ ival 25
        runDimmers getDimmers $ \ds -> do
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
