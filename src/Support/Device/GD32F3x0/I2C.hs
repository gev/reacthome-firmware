{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Support.Device.GD32F3x0.I2C (
    I2C_PERIPH,
    i2c0,
    i2c1,
    DUTYCYCLE,
    i2c_dtcy_2,
    I2C_MODE,
    i2c_i2cmode_enable,
    I2C_FORMAT,
    i2c_addformat_7bits,
    I2C_ACK,
    i2c_ack_enable,
    i2c_ack_disable,
    I2C_FLAG,
    i2c_flag_i2cbsy,
    I2C_INTERRUPT_FLAG,
    i2c_int_flag_sbsend,
    i2c_int_flag_addsend,
    i2c_int_flag_rbne,
    i2c_int_flag_tbe,
    i2c_int_flag_aerr,
    i2c_int_flag_smbalt,
    i2c_int_flag_smbto,
    i2c_int_flag_ouerr,
    i2c_int_flag_lostarb,
    i2c_int_flag_berr,
    i2c_int_flag_pecerr,
    I2C_INTERRUPT,
    i2c_int_err,
    i2c_int_buf,
    i2c_int_ev,
    I2C_TRANSFER_DIRECTION,
    i2c_receiver,
    i2c_transmitter,
    I2C_ACKPOS,
    i2c_ackpos_current,
    i2c_ackpos_next,
    configClockI2C,
    configModeAddrI2C,
    enableI2C,
    configAckI2C,
    getInterruptFlagI2C,
    clearInterruptFlagI2C,
    enableInterruptI2C,
    disableInterruptI2C,
    startOnBusI2C,
    stopOnBusI2C,
    getFlagI2C,
    addressingMasterI2C,
    transmitDataI2C,
    receiveDataI2C,
    configAckposI2C,
    inclI2C,
) where

import Ivory.Language
import Ivory.Support
import Ivory.Support.Device.GD32F3x0

newtype I2C_PERIPH = I2C_PERIPH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
instance ExtSymbol I2C_PERIPH

i2c0 = I2C_PERIPH $ ext "I2C0"
i2c1 = I2C_PERIPH $ ext "I2C1"

newtype DUTYCYCLE = DUTYCYCLE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2c_dtcy_2 = DUTYCYCLE $ ext "I2C_DTCY_2"

newtype I2C_MODE = I2C_MODE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2c_i2cmode_enable = I2C_MODE $ ext "I2C_I2CMODE_ENABLE"

newtype I2C_FORMAT = I2C_FORMAT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2c_addformat_7bits = I2C_FORMAT $ ext "I2C_ADDFORMAT_7BITS"

newtype I2C_ACK = I2C_ACK Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2c_ack_enable = I2C_ACK $ ext "I2C_ACK_ENABLE"
i2c_ack_disable = I2C_ACK $ ext "I2C_ACK_DISABLE"

newtype I2C_FLAG = I2C_FLAG Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2c_flag_i2cbsy = I2C_FLAG $ ext "I2C_FLAG_I2CBSY"

newtype I2C_INTERRUPT_FLAG = I2C_INTERRUPT_FLAG Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2c_int_flag_sbsend = I2C_INTERRUPT_FLAG $ ext "I2C_INT_FLAG_SBSEND"
i2c_int_flag_addsend = I2C_INTERRUPT_FLAG $ ext "I2C_INT_FLAG_ADDSEND"
i2c_int_flag_rbne = I2C_INTERRUPT_FLAG $ ext "I2C_INT_FLAG_RBNE"
i2c_int_flag_tbe = I2C_INTERRUPT_FLAG $ ext "I2C_INT_FLAG_TBE"
i2c_int_flag_aerr = I2C_INTERRUPT_FLAG $ ext "I2C_INT_FLAG_AERR"
i2c_int_flag_smbalt = I2C_INTERRUPT_FLAG $ ext "I2C_INT_FLAG_SMBALT"
i2c_int_flag_smbto = I2C_INTERRUPT_FLAG $ ext "I2C_INT_FLAG_SMBTO"
i2c_int_flag_ouerr = I2C_INTERRUPT_FLAG $ ext "I2C_INT_FLAG_OUERR"
i2c_int_flag_lostarb = I2C_INTERRUPT_FLAG $ ext "I2C_INT_FLAG_LOSTARB"
i2c_int_flag_berr = I2C_INTERRUPT_FLAG $ ext "I2C_INT_FLAG_BERR"
i2c_int_flag_pecerr = I2C_INTERRUPT_FLAG $ ext "I2C_INT_FLAG_PECERR"

newtype I2C_INTERRUPT = I2C_INTERRUPT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2c_int_err = I2C_INTERRUPT $ ext "I2C_INT_ERR"
i2c_int_buf = I2C_INTERRUPT $ ext "I2C_INT_BUF"
i2c_int_ev = I2C_INTERRUPT $ ext "I2C_INT_EV"

newtype I2C_TRANSFER_DIRECTION = I2C_TRANSFER_DIRECTION Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2c_receiver = I2C_TRANSFER_DIRECTION $ ext "I2C_RECEIVER"
i2c_transmitter = I2C_TRANSFER_DIRECTION $ ext "I2C_TRANSMITTER"

newtype I2C_ACKPOS = I2C_ACKPOS Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2c_ackpos_current = I2C_ACKPOS $ ext "I2C_ACKPOS_CURRENT"
i2c_ackpos_next = I2C_ACKPOS $ ext "I2C_ACKPOS_NEXT"

configClockI2C :: I2C_PERIPH -> Uint32 -> DUTYCYCLE -> Ivory eff ()
configClockI2C = call_ i2c_clock_config

i2c_clock_config :: Def ('[I2C_PERIPH, Uint32, DUTYCYCLE] :-> ())
i2c_clock_config = fun "i2c_clock_config"

configModeAddrI2C :: I2C_PERIPH -> I2C_MODE -> I2C_FORMAT -> Uint32 -> Ivory eff ()
configModeAddrI2C = call_ i2c_mode_addr_config

i2c_mode_addr_config :: Def ('[I2C_PERIPH, I2C_MODE, I2C_FORMAT, Uint32] :-> ())
i2c_mode_addr_config = fun "i2c_mode_addr_config"

enableI2C :: I2C_PERIPH -> Ivory eff ()
enableI2C = call_ i2c_enable

i2c_enable :: Def ('[I2C_PERIPH] :-> ())
i2c_enable = fun "i2c_enable"

configAckI2C :: I2C_PERIPH -> I2C_ACK -> Ivory eff ()
configAckI2C = call_ i2c_ack_config

i2c_ack_config :: Def ('[I2C_PERIPH, I2C_ACK] :-> ())
i2c_ack_config = fun "i2c_ack_config"

getInterruptFlagI2C :: I2C_PERIPH -> I2C_INTERRUPT_FLAG -> Ivory eff IBool
getInterruptFlagI2C = call i2c_interrupt_flag_get

i2c_interrupt_flag_get :: Def ('[I2C_PERIPH, I2C_INTERRUPT_FLAG] :-> IBool)
i2c_interrupt_flag_get = fun "i2c_interrupt_flag_get"

clearInterruptFlagI2C :: I2C_PERIPH -> I2C_INTERRUPT_FLAG -> Ivory eff ()
clearInterruptFlagI2C = call_ i2c_interrupt_flag_clear

i2c_interrupt_flag_clear :: Def ('[I2C_PERIPH, I2C_INTERRUPT_FLAG] :-> ())
i2c_interrupt_flag_clear = fun "i2c_interrupt_flag_clear"

enableInterruptI2C :: I2C_PERIPH -> I2C_INTERRUPT -> Ivory eff ()
enableInterruptI2C = call_ i2c_interrupt_enable

i2c_interrupt_enable :: Def ('[I2C_PERIPH, I2C_INTERRUPT] :-> ())
i2c_interrupt_enable = fun "i2c_interrupt_enable"

disableInterruptI2C :: I2C_PERIPH -> I2C_INTERRUPT -> Ivory eff ()
disableInterruptI2C = call_ i2c_interrupt_disable

i2c_interrupt_disable :: Def ('[I2C_PERIPH, I2C_INTERRUPT] :-> ())
i2c_interrupt_disable = fun "i2c_interrupt_disable"

startOnBusI2C :: I2C_PERIPH -> Ivory eff ()
startOnBusI2C = call_ i2c_start_on_bus

i2c_start_on_bus :: Def ('[I2C_PERIPH] :-> ())
i2c_start_on_bus = fun "i2c_start_on_bus"

stopOnBusI2C :: I2C_PERIPH -> Ivory eff ()
stopOnBusI2C = call_ i2c_stop_on_bus

i2c_stop_on_bus :: Def ('[I2C_PERIPH] :-> ())
i2c_stop_on_bus = fun "i2c_stop_on_bus"

getFlagI2C :: I2C_PERIPH -> I2C_FLAG -> Ivory eff ()
getFlagI2C = call_ i2c_flag_get

i2c_flag_get :: Def ('[I2C_PERIPH, I2C_FLAG] :-> ())
i2c_flag_get = fun "i2c_flag_get"

addressingMasterI2C :: I2C_PERIPH -> Uint32 -> I2C_TRANSFER_DIRECTION -> Ivory eff ()
addressingMasterI2C = call_ i2c_master_addressing

i2c_master_addressing :: Def ('[I2C_PERIPH, Uint32, I2C_TRANSFER_DIRECTION] :-> ())
i2c_master_addressing = fun "i2c_master_addressing"

transmitDataI2C :: I2C_PERIPH -> Uint8 -> Ivory eff ()
transmitDataI2C = call_ i2c_data_transmit

i2c_data_transmit :: Def ('[I2C_PERIPH, Uint8] :-> ())
i2c_data_transmit = fun "i2c_data_transmit"

receiveDataI2C :: I2C_PERIPH -> Ivory eff Uint8
receiveDataI2C = call i2c_data_receive

i2c_data_receive :: Def ('[I2C_PERIPH] :-> Uint8)
i2c_data_receive = fun "i2c_data_receive"

configAckposI2C :: I2C_PERIPH -> I2C_ACKPOS -> Ivory eff ()
configAckposI2C = call_ i2c_ackpos_config

i2c_ackpos_config :: Def ('[I2C_PERIPH, I2C_ACKPOS] :-> ())
i2c_ackpos_config = fun "i2c_ackpos_config"

inclI2C :: ModuleDef
inclI2C = do
    incl i2c_clock_config
    incl i2c_mode_addr_config
    incl i2c_enable
    incl i2c_ack_config
    incl i2c_interrupt_flag_get
    incl i2c_interrupt_flag_clear
    incl i2c_interrupt_enable
    incl i2c_interrupt_disable
    incl i2c_start_on_bus
    incl i2c_stop_on_bus
    incl i2c_flag_get
    incl i2c_master_addressing
    incl i2c_data_transmit
    incl i2c_data_receive
    incl i2c_ackpos_config

    inclSym i2c0
    inclSym i2c1

    inclSym i2c_dtcy_2

    inclSym i2c_i2cmode_enable

    inclSym i2c_addformat_7bits

    inclSym i2c_ack_enable
    inclSym i2c_ack_disable

    inclSym i2c_flag_i2cbsy

    inclSym i2c_int_flag_sbsend
    inclSym i2c_int_flag_addsend
    inclSym i2c_int_flag_rbne
    inclSym i2c_int_flag_tbe
    inclSym i2c_int_flag_aerr
    inclSym i2c_int_flag_smbalt
    inclSym i2c_int_flag_smbto
    inclSym i2c_int_flag_ouerr
    inclSym i2c_int_flag_lostarb
    inclSym i2c_int_flag_berr
    inclSym i2c_int_flag_pecerr

    inclSym i2c_int_err
    inclSym i2c_int_buf
    inclSym i2c_int_ev

    inclSym i2c_receiver
    inclSym i2c_transmitter

    inclSym i2c_ackpos_current
    inclSym i2c_ackpos_next
