{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Support.Device.GD32F4xx.ENET
    ( MEDIAMODE
    , enet_auto_negotiation
    
    , CHECKSUM_CONF
    , enet_no_autochecksum

    , FRAME_RECEPT
    , enet_broadcast_frames_pass

    , ENET_INTERRUPT
    , enet_dma_int_nie
    , enet_dma_int_rie

    , ENET_INTERRUPT_FLAG_CLEAR
    , enet_dma_int_flag_rs_clr
    , enet_dma_int_flag_ni_clr

    , initENET
    , deinitENET
    , resetSoftwareENET
    , enableInterruptENET 
    , clearEnetInterruptfFlag
    , getEnetRxframeSize

    , inclENET
    ) where

import           Ivory.Language
import           Ivory.Support.Device.GD32F4xx


newtype MEDIAMODE = MEDIAMODE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

enet_auto_negotiation  = MEDIAMODE $ ext "ENET_AUTO_NEGOTIATION"


newtype CHECKSUM_CONF = CHECKSUM_CONF Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

enet_no_autochecksum  = CHECKSUM_CONF $ ext "ENET_NO_AUTOCHECKSUM"


newtype FRAME_RECEPT = FRAME_RECEPT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

enet_broadcast_frames_pass  = FRAME_RECEPT $ ext "ENET_BROADCAST_FRAMES_PASS"


newtype ENET_INTERRUPT = ENET_INTERRUPT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

enet_dma_int_nie  = ENET_INTERRUPT $ ext "ENET_DMA_INT_NIE"
enet_dma_int_rie  = ENET_INTERRUPT $ ext "ENET_DMA_INT_RIE"


newtype ENET_INTERRUPT_FLAG_CLEAR = ENET_INTERRUPT_FLAG_CLEAR Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

enet_dma_int_flag_rs_clr  = ENET_INTERRUPT_FLAG_CLEAR $ ext "ENET_DMA_INT_FLAG_RS_CLR"
enet_dma_int_flag_ni_clr  = ENET_INTERRUPT_FLAG_CLEAR $ ext "ENET_DMA_INT_FLAG_NI_CLR"


initENET :: MEDIAMODE -> CHECKSUM_CONF -> FRAME_RECEPT -> Ivory eff IBool
initENET = call enet_init

enet_init :: Def ('[MEDIAMODE, CHECKSUM_CONF, FRAME_RECEPT] :-> IBool)
enet_init = fun "enet_init"


deinitENET :: Ivory eff ()
deinitENET = call_ enet_deinit

enet_deinit :: Def ('[] :-> ())
enet_deinit =  fun "enet_deinit"


resetSoftwareENET :: Ivory eff IBool
resetSoftwareENET = call enet_software_reset

enet_software_reset :: Def ('[] :-> IBool)
enet_software_reset = fun "enet_software_reset"


enableInterruptENET :: ENET_INTERRUPT -> Ivory eff ()
enableInterruptENET = call_ enet_interrupt_enable

enet_interrupt_enable :: Def ('[ENET_INTERRUPT] :-> ())
enet_interrupt_enable = fun "enet_interrupt_enable"


clearEnetInterruptfFlag :: ENET_INTERRUPT_FLAG_CLEAR -> Ivory eff ()
clearEnetInterruptfFlag = call_ enet_interrupt_flag_clear

enet_interrupt_flag_clear :: Def ('[ENET_INTERRUPT_FLAG_CLEAR] :-> ())
enet_interrupt_flag_clear = fun "enet_interrupt_flag_clear"


getEnetRxframeSize :: Ivory eff Uint32
getEnetRxframeSize = call enet_rxframe_size_get

enet_rxframe_size_get :: Def ('[] :-> Uint32)
enet_rxframe_size_get = fun "enet_rxframe_size_get"


inclENET :: ModuleDef
inclENET = do
    inclSym enet_auto_negotiation

    inclSym enet_no_autochecksum

    inclSym enet_broadcast_frames_pass

    inclSym enet_dma_int_nie
    inclSym enet_dma_int_rie
    inclSym enet_dma_int_flag_rs_clr
    inclSym enet_dma_int_flag_ni_clr

    incl enet_init
    incl enet_deinit
    incl enet_software_reset
    incl enet_interrupt_enable
    incl enet_interrupt_flag_clear
    incl enet_rxframe_size_get
