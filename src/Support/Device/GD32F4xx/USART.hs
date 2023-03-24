{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F4xx.USART
    ( USART_PERIPH      (..)
    , USART_WORD_LENGTH (..)
    , USART_STOP_BIT    (..)
    , USART_PARITY_CFG  (..)
    , USART_RX_CFG      (..)
    , USART_TX_CFG      (..)
    , USART_FLAG        (..)
    , USART_INT         (..)
    , USART_INT_FLAG    (..)
    , USART_DENT        (..)
    , deinitUSART
    , setWordLength
    , setStopBit
    , configParity
    , setBaudrate
    , configReceive
    , configTransmit
    , enableUSART
    , getFlag
    , receiveData
    , transmitData
    , transmitDMA
    , enableInterrupt
    , disableInterrupt
    , getInterruptFlag
    , clearInterruptFlag
    , tdata
    , inclUSART
    )
where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Support
import           Ivory.Support.Device.GD32F4xx

data USART_INT
    = USART_INT_RBNE
    | USART_INT_TBE
    | USART_INT_TC
    deriving (Show, Enum, Bounded)
instance ExtDef USART_INT Uint32

data USART_INT_FLAG
    = USART_INT_FLAG_RBNE
    | USART_INT_FLAG_TBE
    | USART_INT_FLAG_TC
    deriving (Show, Enum, Bounded)
instance ExtDef USART_INT_FLAG Uint32

data USART_PERIPH
    = USART0
    | USART1
    | USART2
    | USART5
    | UART3
    | UART4
    | UART6
    | UART7
    deriving (Show, Enum, Bounded)
instance ExtDef USART_PERIPH Uint32

data USART_WORD_LENGTH
    = USART_WL_8BIT
    deriving (Show, Enum, Bounded)
instance ExtDef USART_WORD_LENGTH Uint32

data USART_STOP_BIT
    = USART_STB_1BIT
    deriving (Show, Enum, Bounded)
instance ExtDef USART_STOP_BIT Uint32

data USART_PARITY_CFG
    = USART_PM_NONE
    deriving (Show, Enum, Bounded)
instance ExtDef USART_PARITY_CFG Uint32

data USART_RX_CFG
    = USART_RECEIVE_ENABLE
    deriving (Show, Enum, Bounded)
instance ExtDef USART_RX_CFG Uint32

data USART_TX_CFG
    = USART_TRANSMIT_ENABLE
    deriving (Show, Enum, Bounded)
instance ExtDef USART_TX_CFG Uint32

data USART_FLAG
    = USART_FLAG_RBNE
    | USART_FLAG_TBE
    | USART_FLAG_TC
    deriving (Show, Enum, Bounded)
instance ExtDef USART_FLAG Uint32

data USART_DENT
    = USART_DENT_ENABLE
    | USART_DENT_DISABLE
    deriving (Show, Enum, Bounded)
instance ExtDef USART_DENT Uint32


inclUSART :: ModuleDef
inclUSART =    do
    inclDef (def :: Cast USART_PERIPH Uint32)
    inclDef (def :: Cast USART_WORD_LENGTH Uint32)
    inclDef (def :: Cast USART_STOP_BIT Uint32)
    inclDef (def :: Cast USART_PARITY_CFG Uint32)
    inclDef (def :: Cast USART_RX_CFG Uint32)
    inclDef (def :: Cast USART_TX_CFG Uint32)
    inclDef (def :: Cast USART_FLAG Uint32)
    inclDef (def :: Cast USART_INT Uint32)
    inclDef (def :: Cast USART_INT_FLAG Uint32)
    inclDef (def :: Cast USART_DENT Uint32)
    incl usart_deinit
    incl usart_word_length_set
    incl usart_stop_bit_set
    incl usart_parity_config
    incl usart_baudrate_set
    incl usart_receive_config
    incl usart_transmit_config
    incl usart_enable
    incl usart_flag_get
    incl usart_data_receive
    incl usart_data_transmit
    incl usart_interrupt_enable
    incl usart_interrupt_disable
    incl usart_interrupt_flag_get
    incl usart_dma_transmit_config
    incl usart_interrupt_flag_clear
    incl usart_tdata


deinitUSART :: USART_PERIPH -> Ivory eff ()
deinitUSART = call_ usart_deinit . def

usart_deinit :: Def ('[Uint32] :-> ())
usart_deinit = fun "usart_deinit"


setWordLength :: USART_PERIPH -> USART_WORD_LENGTH -> Ivory eff ()
setWordLength usart length =
    call_ usart_word_length_set (def usart) (def length)

usart_word_length_set :: Def ('[Uint32, Uint32] :-> ())
usart_word_length_set = fun "usart_word_length_set"


setStopBit :: USART_PERIPH -> USART_STOP_BIT -> Ivory eff ()
setStopBit usart stopBit =
    call_ usart_stop_bit_set (def usart) (def stopBit)

usart_stop_bit_set :: Def ('[Uint32, Uint32] :-> ())
usart_stop_bit_set = fun "usart_stop_bit_set"


configParity :: USART_PERIPH -> USART_PARITY_CFG -> Ivory eff ()
configParity usart parity =
    call_ usart_parity_config (def usart) (def parity)

usart_parity_config :: Def ('[Uint32, Uint32] :-> ())
usart_parity_config = fun "usart_parity_config"


setBaudrate :: USART_PERIPH -> Uint32 -> Ivory eff ()
setBaudrate usart =
    call_ usart_baudrate_set (def usart)

usart_baudrate_set :: Def ('[Uint32, Uint32] :-> ())
usart_baudrate_set = fun "usart_baudrate_set"


configReceive :: USART_PERIPH -> USART_RX_CFG -> Ivory eff ()
configReceive usart rxconfig =
    call_ usart_receive_config (def usart) (def rxconfig)

usart_receive_config :: Def ('[Uint32, Uint32] :-> ())
usart_receive_config = fun "usart_receive_config"


configTransmit :: USART_PERIPH -> USART_TX_CFG -> Ivory eff ()
configTransmit usart txconfig =
    call_ usart_transmit_config (def usart) (def txconfig)

usart_transmit_config :: Def ('[Uint32, Uint32] :-> ())
usart_transmit_config = fun "usart_transmit_config"


enableUSART :: USART_PERIPH -> Ivory eff ()
enableUSART = call_ usart_enable . def

usart_enable :: Def ('[Uint32] :-> ())
usart_enable = fun "usart_enable"


getFlag :: USART_PERIPH -> USART_FLAG -> Ivory eff IBool
getFlag usart flag =
    call usart_flag_get (def usart) (def flag)

usart_flag_get :: Def ('[Uint32, Uint32] :-> IBool)
usart_flag_get = fun "usart_flag_get"


receiveData :: USART_PERIPH -> Ivory eff Uint16
receiveData = call usart_data_receive . def

usart_data_receive :: Def ('[Uint32] :-> Uint16)
usart_data_receive = fun "usart_data_receive"


transmitData :: USART_PERIPH -> Uint16 -> Ivory eff ()
transmitData = call_ usart_data_transmit . def

usart_data_transmit :: Def ('[Uint32, Uint16] :-> ())
usart_data_transmit = fun "usart_data_transmit"


enableInterrupt :: USART_PERIPH -> USART_INT -> Ivory eff ()
enableInterrupt usart int = call_ usart_interrupt_enable (def usart) (def int)

usart_interrupt_enable :: Def ('[Uint32, Uint32] :-> ())
usart_interrupt_enable = fun "usart_interrupt_enable"


disableInterrupt :: USART_PERIPH -> USART_INT -> Ivory eff ()
disableInterrupt usart int = call_ usart_interrupt_disable (def usart) (def int)

usart_interrupt_disable :: Def ('[Uint32, Uint32] :-> ())
usart_interrupt_disable = fun "usart_interrupt_disable"


getInterruptFlag :: USART_PERIPH -> USART_INT_FLAG -> Ivory eff IBool
getInterruptFlag usart flag = call usart_interrupt_flag_get (def usart) (def flag)

usart_interrupt_flag_get :: Def ('[Uint32, Uint32] :-> IBool)
usart_interrupt_flag_get = fun "usart_interrupt_flag_get"


transmitDMA :: USART_PERIPH -> USART_DENT -> Ivory eff ()
transmitDMA usart dent = call_ usart_dma_transmit_config (def usart) (def dent)

usart_dma_transmit_config :: Def ('[Uint32, Uint32] :-> ())
usart_dma_transmit_config = fun "usart_dma_transmit_config"


tdata :: Uint32 -> Ivory eff Uint32
tdata = call usart_tdata

usart_tdata :: Def ('[Uint32] :-> Uint32)
usart_tdata = fun "(uint32_t) &USART_TDATA"


clearInterruptFlag :: USART_PERIPH -> USART_INT_FLAG -> Ivory eff ()
clearInterruptFlag usart flag = call_ usart_interrupt_flag_clear (def usart) (def flag)

usart_interrupt_flag_clear :: Def ('[Uint32, Uint32] :-> ())
usart_interrupt_flag_clear = fun "usart_interrupt_flag_clear"
