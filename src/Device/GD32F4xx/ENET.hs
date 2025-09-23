{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Device.GD32F4xx.ENET where

import qualified Control.Monad
import Control.Monad.State (MonadState)
import Core.Context
import Core.Handler
import qualified Device.GD32F4xx.GPIO.Port as G
import Interface.ENET as I
import Interface.LwipPort
import Ivory.Stdlib
import Support.Device.GD32F4xx.ENET
import Support.Device.GD32F4xx.GPIO
import Support.Device.GD32F4xx.IRQ
import Support.Device.GD32F4xx.LwipPort.Basic.Ethernetif
import Support.Device.GD32F4xx.Misc
import Support.Device.GD32F4xx.RCU
import Support.Device.GD32F4xx.SYSCFG

newtype ENET = ENET {enetIRQ :: IRQn}

mkENET ::
    (MonadState Context m) =>
    (GPIO_PUPD -> G.Port) ->
    (GPIO_PUPD -> G.Port) ->
    (GPIO_PUPD -> G.Port) ->
    (GPIO_PUPD -> G.Port) ->
    (GPIO_PUPD -> G.Port) ->
    (GPIO_PUPD -> G.Port) ->
    (GPIO_PUPD -> G.Port) ->
    (GPIO_PUPD -> G.Port) ->
    (GPIO_PUPD -> G.Port) ->
    IRQn ->
    m ENET
mkENET ethRmiiRefClk ethRmiiMdio ethRmiiMdc ethRmiiCrsDv ethRmiiRxd0 ethRmiiRxd1 ethRmiiTxEn ethRmiiTxd0 ethRmiiTxd1 enetIRQ = do
    G.initPort $ ethRmiiRefClk gpio_pupd_none
    G.initPort $ ethRmiiMdio gpio_pupd_none
    G.initPort $ ethRmiiMdc gpio_pupd_none
    G.initPort $ ethRmiiCrsDv gpio_pupd_none
    G.initPort $ ethRmiiRxd0 gpio_pupd_none
    G.initPort $ ethRmiiRxd1 gpio_pupd_none
    G.initPort $ ethRmiiTxEn gpio_pupd_none
    G.initPort $ ethRmiiTxd0 gpio_pupd_none
    G.initPort $ ethRmiiTxd1 gpio_pupd_none

    addModule inclEthernetif

    addInit "enet_" $ do
        -- setVectorTableNvic nvic_vecttab_flash 0
        -- setPriorityGroup nvic_prigroup_pre2_sub2
        -- enableIrqNvic enetIRQ 0 0

        enablePeriphClock rcu_syscfg
        configPhyInterface enet_phy_rmii

        enablePeriphClock rcu_enet
        enablePeriphClock rcu_enettx
        enablePeriphClock rcu_enetrx
        deinitENET
        resetSoftwareENET
        initENET enet_100m_fullduplex enet_no_autochecksum enet_broadcast_frames_pass
        enableEnetFilterFeature enet_multicast_filter_pass
    -- when isReady $ do
    --     enableInterruptENET enet_dma_int_nie
    --     enableInterruptENET enet_dma_int_rie

    pure ENET{enetIRQ}

instance Handler I.HandleEnet ENET where
    addHandler (I.HandleEnet ENET{..} handle) = do
        addModule $ makeIRQHandler enetIRQ $ handleENET handle

handleENET handle = do
    handle
    clearEnetInterruptfFlag enet_dma_int_flag_rs_clr
    clearEnetInterruptfFlag enet_dma_int_flag_ni_clr

instance I.Enet ENET where
    rxFrameSize _ = getEnetRxframeSize

instance LwipPort ENET where
    initLwipPortIf _ = initEthernetifPtr

    inputLwipPortIf _ = inputEthernetif
