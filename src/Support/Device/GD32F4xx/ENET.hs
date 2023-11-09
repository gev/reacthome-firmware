{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Support.Device.GD32F4xx.ENET
    ( MEDIAMODE
    , auto_negotiation
    
    , CHECKSUM_CONF
    , no_autochecksum

    , FRAME_RECEPT
    , broadcast_frames_pass

    , ENET_INTERRUPT
    , enet_dma_int_nie
    , enet_dma_int_rie

    , initENET
    , deinitENET
    , resetSoftwareENET
    , enableInterruptENET 

    , inclENET
    ) where

import           Ivory.Language
import           Ivory.Support.Device.GD32F4xx


newtype MEDIAMODE = MEDIAMODE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

auto_negotiation  = MEDIAMODE $ ext "ENET_AUTO_NEGOTIATION"


newtype CHECKSUM_CONF = CHECKSUM_CONF Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

no_autochecksum  = CHECKSUM_CONF $ ext "ENET_NO_AUTOCHECKSUM"


newtype FRAME_RECEPT = FRAME_RECEPT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

broadcast_frames_pass  = FRAME_RECEPT $ ext "ENET_BROADCAST_FRAMES_PASS"


newtype ENET_INTERRUPT = ENET_INTERRUPT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

enet_dma_int_nie  = ENET_INTERRUPT $ ext "ENET_DMA_INT_NIE"
enet_dma_int_rie  = ENET_INTERRUPT $ ext "ENET_DMA_INT_RIE"


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


inclENET :: ModuleDef
inclENET = do
    inclSym auto_negotiation

    inclSym no_autochecksum

    inclSym broadcast_frames_pass

    inclSym enet_dma_int_nie
    inclSym enet_dma_int_rie

    incl enet_init
    incl enet_deinit
    incl enet_software_reset
    incl enet_interrupt_enable
