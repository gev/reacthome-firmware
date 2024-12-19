{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F4xx.USART
    ( USART_INT
    , usart_int_rbne
    , usart_int_tbe
    , usart_int_tc
    , usart_int_perr
    , usart_int_err

    , USART_INT_FLAG
    , usart_int_flag_rbne
    , usart_int_flag_tbe
    , usart_int_flag_tc
    , usart_int_flag_rbne_orerr
    , usart_int_flag_err_orerr
    , usart_int_flag_err_nerr
    , usart_int_flag_err_ferr
    , usart_int_flag_perr

    , USART_PERIPH
    , usart0
    , usart1
    , usart2
    , usart5
    , uart3
    , uart4
    , uart6
    , uart7

    , USART_WORD_LENGTH
    , usart_wl_8bit
    , usart_wl_9bit

    , USART_STOP_BIT
    , usart_stb_0_5bit
    , usart_stb_1bit
    , usart_stb_1_5bit
    , usart_stb_2bit

    , USART_PARITY_CFG
    , usart_pm_none
    , usart_pm_even
    , usart_pm_odd

    , USART_RX_CFG
    , usart_receive_enable

    , USART_TX_CFG
    , usart_transmit_enable

    , USART_FLAG
    , usart_flag_rbne
    , usart_flag_tbe
    , usart_flag_tc
    , usart_flag_ferr
    , usart_flag_nerr
    , usart_flag_perr
    , usart_flag_orerr

    , USART_DENT
    , usart_dent_enable
    , usart_dent_disable

    , deinitUSART
    , setWordLength
    , setStopBit
    , configParity
    , setBaudrate
    , configReceive
    , configTransmit
    , enableUSART
    , getFlag
    , clearFlag
    , receiveData
    , transmitData
    , transmitDMA
    , enableInterrupt
    , disableInterrupt
    , getInterruptFlag
    , clearInterruptFlag

    , udata

    , inclUSART
    )
where

import           Ivory.Language
import           Ivory.Support
import           Ivory.Support.Device.GD32F4xx



newtype USART_INT = USART_INT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

usart_int_rbne      = USART_INT $ ext "USART_INT_RBNE"
usart_int_tbe       = USART_INT $ ext "USART_INT_TBE"
usart_int_tc        = USART_INT $ ext "USART_INT_TC"
usart_int_perr      = USART_INT $ ext "USART_INT_PERR"
usart_int_err       = USART_INT $ ext "USART_INT_ERR"



newtype USART_INT_FLAG = USART_INT_FLAG Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

usart_int_flag_rbne         = USART_INT_FLAG $ ext "USART_INT_FLAG_RBNE"
usart_int_flag_tbe          = USART_INT_FLAG $ ext "USART_INT_FLAG_TBE"
usart_int_flag_tc           = USART_INT_FLAG $ ext "USART_INT_FLAG_TC"
usart_int_flag_rbne_orerr   = USART_INT_FLAG $ ext "USART_INT_FLAG_RBNE_ORERR"
usart_int_flag_err_orerr    = USART_INT_FLAG $ ext "USART_INT_FLAG_ERR_ORERR"
usart_int_flag_err_nerr     = USART_INT_FLAG $ ext "USART_INT_FLAG_ERR_NERR"
usart_int_flag_err_ferr     = USART_INT_FLAG $ ext "USART_INT_FLAG_ERR_FERR"
usart_int_flag_perr         = USART_INT_FLAG $ ext "USART_INT_FLAG_PERR"




newtype USART_PERIPH = USART_PERIPH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
instance ExtSymbol USART_PERIPH

usart0 = USART_PERIPH $ ext "USART0"
usart1 = USART_PERIPH $ ext "USART1"
usart2 = USART_PERIPH $ ext "USART2"
usart5 = USART_PERIPH $ ext "USART5"
uart3  = USART_PERIPH $ ext "UART3"
uart4  = USART_PERIPH $ ext "UART4"
uart6  = USART_PERIPH $ ext "UART6"
uart7  = USART_PERIPH $ ext "UART7"



newtype USART_WORD_LENGTH = USART_WORD_LENGTH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

usart_wl_8bit = USART_WORD_LENGTH $ ext "USART_WL_8BIT"
usart_wl_9bit = USART_WORD_LENGTH $ ext "USART_WL_9BIT"



newtype USART_STOP_BIT = USART_STOP_BIT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

usart_stb_0_5bit = USART_STOP_BIT $ ext "USART_STB_0_5BIT"
usart_stb_1bit = USART_STOP_BIT $ ext "USART_STB_1BIT"
usart_stb_1_5bit = USART_STOP_BIT $ ext "USART_STB_1_5BIT"
usart_stb_2bit = USART_STOP_BIT $ ext "USART_STB_2BIT"



newtype USART_PARITY_CFG = USART_PARITY_CFG Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

usart_pm_none = USART_PARITY_CFG $ ext "USART_PM_NONE"
usart_pm_even = USART_PARITY_CFG $ ext "USART_PM_EVEN"
usart_pm_odd  = USART_PARITY_CFG $ ext "USART_PM_ODD"



newtype USART_RX_CFG = USART_RX_CFG Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

usart_receive_enable = USART_RX_CFG $ ext "USART_RECEIVE_ENABLE"



newtype USART_TX_CFG = USART_TX_CFG Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

usart_transmit_enable = USART_TX_CFG$ ext "USART_TRANSMIT_ENABLE"



newtype USART_FLAG = USART_FLAG Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

usart_flag_rbne   = USART_FLAG $ ext "USART_FLAG_RBNE"
usart_flag_tbe    = USART_FLAG $ ext "USART_FLAG_TBE"
usart_flag_tc     = USART_FLAG $ ext "USART_FLAG_TC"
usart_flag_ferr   = USART_FLAG $ ext "USART_FLAG_FERR"
usart_flag_nerr   = USART_FLAG $ ext "USART_FLAG_NERR"
usart_flag_perr   = USART_FLAG $ ext "USART_FLAG_PERR"
usart_flag_orerr  = USART_FLAG $ ext "USART_FLAG_ORERR"



newtype USART_DENT = USART_DENT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

usart_dent_enable  = USART_DENT $ ext "USART_DENT_ENABLE"
usart_dent_disable = USART_DENT $ ext "USART_DENT_DISABLE"



deinitUSART :: USART_PERIPH -> Ivory eff ()
deinitUSART = call_ usart_deinit

usart_deinit :: Def ('[USART_PERIPH] :-> ())
usart_deinit = fun "usart_deinit"


setWordLength :: USART_PERIPH -> USART_WORD_LENGTH -> Ivory eff ()
setWordLength = call_ usart_word_length_set

usart_word_length_set :: Def ('[USART_PERIPH, USART_WORD_LENGTH] :-> ())
usart_word_length_set = fun "usart_word_length_set"


setStopBit :: USART_PERIPH -> USART_STOP_BIT -> Ivory eff ()
setStopBit = call_ usart_stop_bit_set

usart_stop_bit_set :: Def ('[USART_PERIPH, USART_STOP_BIT] :-> ())
usart_stop_bit_set = fun "usart_stop_bit_set"


configParity :: USART_PERIPH -> USART_PARITY_CFG -> Ivory eff ()
configParity = call_ usart_parity_config

usart_parity_config :: Def ('[USART_PERIPH, USART_PARITY_CFG] :-> ())
usart_parity_config = fun "usart_parity_config"


setBaudrate :: USART_PERIPH -> Uint32 -> Ivory eff ()
setBaudrate = call_ usart_baudrate_set

usart_baudrate_set :: Def ('[USART_PERIPH, Uint32] :-> ())
usart_baudrate_set = fun "usart_baudrate_set"


configReceive :: USART_PERIPH -> USART_RX_CFG -> Ivory eff ()
configReceive = call_ usart_receive_config

usart_receive_config :: Def ('[USART_PERIPH, USART_RX_CFG] :-> ())
usart_receive_config = fun "usart_receive_config"


configTransmit :: USART_PERIPH -> USART_TX_CFG -> Ivory eff ()
configTransmit = call_ usart_transmit_config

usart_transmit_config :: Def ('[USART_PERIPH, USART_TX_CFG] :-> ())
usart_transmit_config = fun "usart_transmit_config"


enableUSART :: USART_PERIPH -> Ivory eff ()
enableUSART = call_ usart_enable

usart_enable :: Def ('[USART_PERIPH] :-> ())
usart_enable = fun "usart_enable"


getFlag :: USART_PERIPH -> USART_FLAG -> Ivory eff IBool
getFlag = call usart_flag_get

usart_flag_get :: Def ('[USART_PERIPH, USART_FLAG] :-> IBool)
usart_flag_get = fun "usart_flag_get"


receiveData :: USART_PERIPH -> Ivory eff Uint16
receiveData = call usart_data_receive

usart_data_receive :: Def ('[USART_PERIPH] :-> Uint16)
usart_data_receive = fun "usart_data_receive"


transmitData :: USART_PERIPH -> Uint16 -> Ivory eff ()
transmitData = call_ usart_data_transmit

usart_data_transmit :: Def ('[USART_PERIPH, Uint16] :-> ())
usart_data_transmit = fun "usart_data_transmit"


enableInterrupt :: USART_PERIPH -> USART_INT -> Ivory eff ()
enableInterrupt = call_ usart_interrupt_enable

usart_interrupt_enable :: Def ('[USART_PERIPH, USART_INT] :-> ())
usart_interrupt_enable = fun "usart_interrupt_enable"


disableInterrupt :: USART_PERIPH -> USART_INT -> Ivory eff ()
disableInterrupt = call_ usart_interrupt_disable

usart_interrupt_disable :: Def ('[USART_PERIPH, USART_INT] :-> ())
usart_interrupt_disable = fun "usart_interrupt_disable"


getInterruptFlag :: USART_PERIPH -> USART_INT_FLAG -> Ivory eff IBool
getInterruptFlag = call usart_interrupt_flag_get

usart_interrupt_flag_get :: Def ('[USART_PERIPH, USART_INT_FLAG] :-> IBool)
usart_interrupt_flag_get = fun "usart_interrupt_flag_get"


transmitDMA :: USART_PERIPH -> USART_DENT -> Ivory eff ()
transmitDMA = call_ usart_dma_transmit_config

usart_dma_transmit_config :: Def ('[USART_PERIPH, USART_DENT] :-> ())
usart_dma_transmit_config = fun "usart_dma_transmit_config"


udata :: USART_PERIPH -> Ivory eff Uint32
udata = call usart_udata

usart_udata :: Def ('[USART_PERIPH] :-> Uint32)
usart_udata = fun "(uint32_t) &USART_DATA"


clearInterruptFlag :: USART_PERIPH -> USART_INT_FLAG -> Ivory eff ()
clearInterruptFlag = call_ usart_interrupt_flag_clear

usart_interrupt_flag_clear :: Def ('[USART_PERIPH, USART_INT_FLAG] :-> ())
usart_interrupt_flag_clear = fun "usart_interrupt_flag_clear"


clearFlag :: USART_PERIPH -> USART_FLAG -> Ivory eff ()
clearFlag = call_ usart_flag_clear

usart_flag_clear :: Def ('[USART_PERIPH, USART_FLAG] :-> ())
usart_flag_clear = fun "usart_flag_clear"



inclUSART :: ModuleDef
inclUSART = do
    inclSym usart_int_rbne
    inclSym usart_int_tbe
    inclSym usart_int_tc
    inclSym usart_int_perr
    inclSym usart_int_err

    inclSym usart_int_flag_rbne
    inclSym usart_int_flag_tbe
    inclSym usart_int_flag_tc
    inclSym usart_int_flag_rbne_orerr
    inclSym usart_int_flag_err_orerr 
    inclSym usart_int_flag_err_nerr  
    inclSym usart_int_flag_err_ferr  
    inclSym usart_int_flag_perr      

    inclSym usart0
    inclSym usart1
    inclSym usart2
    inclSym usart5

    inclSym uart3
    inclSym uart4
    inclSym uart6
    inclSym uart7

    inclSym usart_wl_8bit
    inclSym usart_wl_9bit

    inclSym usart_stb_0_5bit
    inclSym usart_stb_1bit
    inclSym usart_stb_1_5bit
    inclSym usart_stb_2bit

    inclSym usart_pm_none
    inclSym usart_pm_even
    inclSym usart_pm_odd

    inclSym usart_receive_enable

    inclSym usart_transmit_enable

    inclSym usart_flag_rbne
    inclSym usart_flag_tbe
    inclSym usart_flag_tc
    inclSym usart_flag_ferr
    inclSym usart_flag_nerr
    inclSym usart_flag_perr
    inclSym usart_flag_orerr

    inclSym usart_dent_enable
    inclSym usart_dent_disable

    incl usart_deinit
    incl usart_word_length_set
    incl usart_stop_bit_set
    incl usart_parity_config
    incl usart_baudrate_set
    incl usart_receive_config
    incl usart_transmit_config
    incl usart_enable
    incl usart_flag_get
    incl usart_flag_clear
    incl usart_data_receive
    incl usart_data_transmit
    incl usart_interrupt_enable
    incl usart_interrupt_disable
    incl usart_interrupt_flag_get
    incl usart_dma_transmit_config
    incl usart_interrupt_flag_clear
    incl usart_udata
