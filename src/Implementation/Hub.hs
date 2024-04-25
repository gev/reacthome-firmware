{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Implementation.Hub where

import           Control.Monad           (zipWithM_)
import           Control.Monad.Reader    (MonadReader, asks)
import           Core.Actions
import           Core.Controller         (Controller, handle)
import qualified Core.Domain             as D
import           Data.Buffer
import           Data.Serialize
import           Data.Value
import           Endpoint.Dimmers        (initialize, runDimmers,
                                          syncDimmerGroup)
import           Feature.Dimmers         (Dimmers (getDimmers), forceSync, n,
                                          onDim, onDo)
import           Feature.DInputs         (DInputs, forceSyncDInputs)
import           Feature.DS18B20         (DS18B20)
import           Feature.Indicator       (Indicator, onFindMe)
import           Feature.RS485.RBUS      (configureMode, forceSyncRBUS',
                                          setMode, transmitRB485, transmitRBUS)
import           Feature.RS485.RBUS.Data (RBUS (..))
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib



data Hub = Hub
    { rbus       :: [RBUS]
    , dimmers    :: Dimmers
    , dinputs    :: DInputs
    , ds18b20    :: DS18B20
    -- , indicator  :: Indicator 20
    , shouldInit :: Value IBool
    }



hub :: MonadReader (D.Domain p c) m
    => m t -> (t -> m [RBUS]) -> (t -> m Dimmers) -> (Bool -> t -> m DInputs) -> (t -> m DS18B20) -> (t -> m (Indicator 20)) -> m Hub
hub transport' rbus' dimmers' dinputs' ds18b20' indicator' = do
    transport  <- transport'
    rbus       <- rbus' transport
    dimmers    <- dimmers' transport
    dinputs    <- dinputs' True transport
    ds18b20    <- ds18b20' transport
    -- indicator  <- indicator' transport
    shouldInit <- asks D.shouldInit
    pure Hub { rbus, dimmers, dinputs, ds18b20, shouldInit }



instance Controller Hub where
    handle s@Hub{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionDo            ==> onDo          dimmers   buff size
              , action ==? actionDim           ==> onDim         dimmers   buff size
              , action ==? actionRs485Mode     ==> setMode       rbus      buff size
              , action ==? actionRbusTransmit  ==> transmitRBUS  rbus      buff size
              , action ==? actionRs485Transmit ==> transmitRB485 rbus      buff size
            --   , action ==? actionFindMe        ==> onFindMe      indicator buff size
              , action ==? actionInitialize    ==> onInit        s         buff size
              , action ==? actionGetState      ==> onGetState    s
              ]


onInit :: KnownNat n => Hub -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onInit Hub{..} buff size =
    when (size ==? 25 + n dimmers * 3) $ do

        let run r@RBUS{..} offset = do
                store mode        =<< unpack   buff  offset
                store baudrate    =<< unpackLE buff (offset + 1)
                store lineControl =<< unpack   buff (offset + 5)
                configureMode r
        zipWithM_ run rbus $ fromIntegral <$> [1, 7..]

        offset <- local $ ival 25
        runDimmers (getDimmers dimmers) $ \ds -> do
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



onGetState Hub{..} = do
    forceSyncDInputs dinputs
    initialized <- iNot <$> deref shouldInit
    when initialized $ do
        forceSync dimmers
        forceSyncRBUS' rbus
