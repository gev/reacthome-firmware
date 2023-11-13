{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Device.GD32F4xx.ENET where

import           Control.Monad.State            (MonadState)
import           Core.Context
import           Core.Handler
import qualified Device.GD32F4xx.GPIO.Port      as G
import           Interface.ENET                 as I
import           Ivory.Stdlib
import           Support.Device.GD32F4xx.ENET
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.Misc
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.SYSCFG
import qualified Control.Monad



data ENET = ENET
    { ethRmiiRefClk :: G.Port
    , ethRmiiMdio   :: G.Port
    , ethRmiiMdc    :: G.Port
    , ethRmiiCrsDv  :: G.Port
    , ethRmiiRxd0   :: G.Port
    , ethRmiiRxd1   :: G.Port
    , ethRmiiTxEn   :: G.Port
    , ethRmiiTxd0   :: G.Port
    , ethRmiiTxd1   :: G.Port
    , enetIRQ       :: IRQn
    }

mkENET :: MonadState Context m
       => G.Port
       -> G.Port
       -> G.Port
       -> G.Port
       -> G.Port
       -> G.Port
       -> G.Port
       -> G.Port
       -> G.Port
       -> IRQn
       -> m ENET
mkENET ethRmiiRefClk ethRmiiMdio ethRmiiMdc ethRmiiCrsDv ethRmiiRxd0 ethRmiiRxd1 ethRmiiTxEn ethRmiiTxd0 ethRmiiTxd1 enetIRQ= do
    G.initPort ethRmiiRefClk
    G.initPort ethRmiiMdio
    G.initPort ethRmiiMdc
    G.initPort ethRmiiCrsDv
    G.initPort ethRmiiRxd0
    G.initPort ethRmiiRxd1
    G.initPort ethRmiiTxEn
    G.initPort ethRmiiTxd0
    G.initPort ethRmiiTxd1

    addInit "enet_" $ do
        setVectorTableNvic nvic_vecttab_flash 0
        setPriorityGroup nvic_prigroup_pre2_sub2
        enableIrqNvic enetIRQ 0 0

        enablePeriphClock rcu_syscfg
        configPhyInterface enet_phy_rmii

        enablePeriphClock rcu_enet
        enablePeriphClock rcu_enettx
        enablePeriphClock rcu_enetrx
        deinitENET
        resetSoftwareENET
        isReady <- initENET enet_auto_negotiation enet_no_autochecksum enet_broadcast_frames_pass
        when isReady $ do
            enableInterruptENET enet_dma_int_nie
            enableInterruptENET enet_dma_int_rie

    pure ENET { ethRmiiRefClk, ethRmiiMdio, ethRmiiMdc, ethRmiiCrsDv, ethRmiiRxd0, ethRmiiRxd1, ethRmiiTxEn, ethRmiiTxd0, ethRmiiTxd1, enetIRQ }



instance Handler I.HandleEnet ENET where
    addHandler (I.HandleEnet ENET{..} handle) = do
        addModule $ makeIRQHandler enetIRQ $ handleENET handle

handleENET handle = do
    clearEnetInterruptfFlag enet_dma_int_flag_rs_clr
    clearEnetInterruptfFlag enet_dma_int_flag_ni_clr
    handle

instance I.Enet ENET where
    rxFrameSize _ = getEnetRxframeSize
