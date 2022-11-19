{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Support.Device.GD32F3x0.USART
  ( USART_PERIPH      (..)
  , USART_WORD_LENGHT (..)
  , USART_STOP_BIT    (..)
  , USART_PARITY_CFG  (..)
  , USART_RX_CFG      (..)
  , USART_TX_CFG      (..)
  , USART_FLAG        (..)
  , deinitUSART
  , wordLengthUSART
  , stopBitSetUSART
  , baudrateSetUSART
  , receiveConfigUSART
  , transmitConfigUSART
  , enableUSART
  , getFlag
  , receiveData
  , transmitData
  , inclUSART
  )
where

import           Ivory.Language
import           Ivory.Language.Module
import           Support.Ivory

(def, fun) = include "gd32f3x0_usart.h"


data USART_PERIPH
  = USART0
  | USART1
  deriving (Show, Enum, Bounded)
instance ExtDef USART_PERIPH Uint32

data USART_WORD_LENGHT
  = USART_WL_8BIT
  deriving (Show, Enum, Bounded)
instance ExtDef USART_WORD_LENGHT Uint32

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


inclUSART :: ModuleM ()
inclUSART = do
  inclDef (def :: Cast USART_PERIPH Uint32)
  inclDef (def :: Cast USART_WORD_LENGHT Uint32)
  inclDef (def :: Cast USART_STOP_BIT Uint32)
  inclDef (def :: Cast USART_PARITY_CFG Uint32)
  inclDef (def :: Cast USART_RX_CFG Uint32)
  inclDef (def :: Cast USART_TX_CFG Uint32)
  inclDef (def :: Cast USART_FLAG Uint32)
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


deinitUSART :: USART_PERIPH -> Ivory eff ()
deinitUSART = call_ usart_deinit . def

usart_deinit :: Def ('[Uint32] :-> ())
usart_deinit = fun "usart_deinit"


wordLengthUSART :: USART_PERIPH -> USART_WORD_LENGHT -> Ivory eff ()
wordLengthUSART usart length =
  call_ usart_word_length_set (def usart) (def length)

usart_word_length_set :: Def ('[Uint32, Uint32] :-> ())
usart_word_length_set = fun "usart_word_length_set"


stopBitSetUSART :: USART_PERIPH -> USART_STOP_BIT -> Ivory eff ()
stopBitSetUSART usart stopBit =
  call_ usart_stop_bit_set (def usart) (def stopBit)

usart_stop_bit_set :: Def ('[Uint32, Uint32] :-> ())
usart_stop_bit_set = fun "usart_stop_bit_set"


parityConfigUSART :: USART_PERIPH -> USART_PARITY_CFG -> Ivory eff ()
parityConfigUSART usart parity =
  call_ usart_parity_config (def usart) (def parity)

usart_parity_config :: Def ('[Uint32, Uint32] :-> ())
usart_parity_config = fun "usart_parity_config"


baudrateSetUSART :: USART_PERIPH -> Uint32 -> Ivory eff ()
baudrateSetUSART usart =
  call_ usart_baudrate_set (def usart)

usart_baudrate_set :: Def ('[Uint32, Uint32] :-> ())
usart_baudrate_set = fun "usart_baudrate_set"


receiveConfigUSART :: USART_PERIPH -> USART_RX_CFG -> Ivory eff ()
receiveConfigUSART usart rxconfig =
  call_ usart_receive_config (def usart) (def rxconfig)

usart_receive_config :: Def ('[Uint32, Uint32] :-> ())
usart_receive_config = fun "usart_receive_config"


transmitConfigUSART :: USART_PERIPH -> USART_TX_CFG -> Ivory eff ()
transmitConfigUSART usart txconfig =
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


transmitData :: USART_PERIPH -> Uint32 -> Ivory eff ()
transmitData = call_ usart_data_transmit . def

usart_data_transmit :: Def ('[Uint32, Uint32] :-> ())
usart_data_transmit = fun "usart_data_transmit"

