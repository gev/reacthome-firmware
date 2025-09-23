{-# LANGUAGE RecordWildCards #-}

module Device.GD32F4xx.I2C where

import Control.Monad.State (MonadState)
import Core.Context
import Core.Handler
import Data.Buffer
import Data.Value
import Device.GD32F4xx.GPIO.Port hiding (mode, rcu)
import GHC.TypeNats
import qualified Interface.I2C as I
import Ivory.Language
import Ivory.Stdlib
import Ivory.Support
import Support.Device.GD32F4xx.GPIO
import Support.Device.GD32F4xx.I2C
import Support.Device.GD32F4xx.IRQ
import Support.Device.GD32F4xx.Misc
import Support.Device.GD32F4xx.RCU

data I2C n = I2C
    { i2c :: I2C_PERIPH
    , eventIrq :: IRQn
    , errorIrq :: IRQn
    , mode :: Value IBool
    , address :: Value Uint8
    , txBuff :: Buffer n Uint8
    , index :: Value Uint16
    , size :: Value Sint32
    }

mkI2C ::
    (MonadState Context m, KnownNat n) =>
    I2C_PERIPH ->
    RCU_PERIPH ->
    IRQn ->
    IRQn ->
    (GPIO_PUPD -> Port) ->
    (GPIO_PUPD -> Port) ->
    m (I2C n)
mkI2C i2c rcu eventIrq errorIrq sda' scl' = do
    mode <- value_ $ symbol i2c <> "_mode"
    address <- value_ $ symbol i2c <> "_address"
    txBuff <- buffer $ symbol i2c <> "_tx_buff"
    index <- value_ $ symbol i2c <> "_tx_index"
    size <- value_ $ symbol i2c <> "_tx_size"

    let sda = sda' gpio_pupd_none
    let scl = scl' gpio_pupd_none

    initPort sda
    initPort scl

    addInit (symbol i2c) $ do
        enablePeriphClock rcu
        configClockI2C i2c 400000 i2c_dtcy_2
        configModeAddrI2C i2c i2c_i2cmode_enable i2c_addformat_7bits 0
        enableI2C i2c
        -- configAckI2C i2c i2c_ack_enable
        enableIrqNvic eventIrq 0 0
        enableIrqNvic errorIrq 0 1

    pure I2C{i2c, eventIrq, errorIrq, mode, address, txBuff, index, size}

instance (KnownNat n) => I.I2C I2C n where
    receive I2C{..} addr len = do
        store size $ safeCast len
        store address addr
        store mode false
        store index 0
        configAckI2C i2c i2c_ack_enable
        enableInterruptI2C i2c i2c_int_ev
        enableInterruptI2C i2c i2c_int_buf
        enableInterruptI2C i2c i2c_int_err
        startOnBusI2C i2c

    transmit I2C{..} addr buff = do
        arrayMap $ \ix -> store (txBuff ! ix) =<< deref (buff ! ix)
        store size $ arrayLen buff
        store address addr
        store mode true
        store index 0
        enableInterruptI2C i2c i2c_int_ev
        enableInterruptI2C i2c i2c_int_buf
        enableInterruptI2C i2c i2c_int_err
        startOnBusI2C i2c

instance (KnownNat n) => Handler I.HandleI2C (I2C n) where
    addHandler I.HandleI2C{..} = do
        addModule $ makeIRQHandler (eventIrq i2c) (handleEvent i2c handle)
        addModule $ makeIRQHandler (errorIrq i2c) (handleError i2c)

handleEvent :: (KnownNat n) => I2C n -> (Uint8 -> Uint16 -> Ivory eff ()) -> Ivory eff ()
handleEvent i2c receive = do
    mode' <- deref $ mode i2c
    ifte_
        mode'
        (handleTransmit i2c)
        (handleReceive i2c receive)

handleTransmit :: (KnownNat n) => I2C n -> Ivory eff ()
handleTransmit I2C{..} = do
    sbsend <- getInterruptFlagI2C i2c i2c_int_flag_sbsend
    ifte_
        sbsend
        ( do
            {-
                Should clear i2c_int_flag_sbsend?
            -}
            address' <- safeCast <$> deref address
            addressingMasterI2C i2c address' i2c_transmitter
        )
        ( do
            addsend <- getInterruptFlagI2C i2c i2c_int_flag_addsend
            ifte_
                addsend
                (clearInterruptFlagI2C i2c i2c_int_flag_addsend)
                ( do
                    tbe <- getInterruptFlagI2C i2c i2c_int_flag_tbe
                    when tbe $ do
                        index' <- deref index
                        size' <- deref size
                        ifte_
                            (safeCast index' <? size')
                            ( do
                                transmitDataI2C i2c =<< deref (txBuff ! toIx index')
                                store index $ index' + 1
                            )
                            ( do
                                stopOnBusI2C i2c
                                disableInterruptI2C i2c i2c_int_err
                                disableInterruptI2C i2c i2c_int_buf
                                disableInterruptI2C i2c i2c_int_ev
                            )
                )
        )

handleReceive :: I2C n -> (Uint8 -> Uint16 -> Ivory eff a) -> Ivory eff ()
handleReceive I2C{..} receive = do
    sbsend <- getInterruptFlagI2C i2c i2c_int_flag_sbsend
    ifte_
        sbsend
        ( do
            {-
                Should clear i2c_int_flag_sbsend?
            -}
            address' <- safeCast <$> deref address
            addressingMasterI2C i2c address' i2c_receiver
        )
        ( do
            addsend <- getInterruptFlagI2C i2c i2c_int_flag_addsend
            ifte_
                addsend
                ( do
                    size' <- deref size
                    when (size' ==? 1) $ do
                        configAckI2C i2c i2c_ack_disable
                    clearInterruptFlagI2C i2c i2c_int_flag_addsend
                )
                ( do
                    size' <- deref size
                    rbne <- getInterruptFlagI2C i2c i2c_int_flag_rbne
                    when rbne $ do
                        cond_
                            [ size' ==? 2 ==> configAckI2C i2c i2c_ack_disable
                            , size' ==? 1 ==> do
                                stopOnBusI2C i2c
                                configAckposI2C i2c i2c_ackpos_current
                                disableInterruptI2C i2c i2c_int_err
                                disableInterruptI2C i2c i2c_int_buf
                                disableInterruptI2C i2c i2c_int_ev
                            ]

                        store size $ size' - 1

                        index' <- deref index
                        v <- receiveDataI2C i2c
                        receive v index'
                        store index $ index' + 1

                        clearInterruptFlagI2C i2c i2c_int_flag_rbne
                )
        )

handleError :: I2C n -> Ivory eff ()
handleError I2C{..} = do
    clearInterruptFlagI2C i2c i2c_int_flag_aerr
    clearInterruptFlagI2C i2c i2c_int_flag_smbalt
    clearInterruptFlagI2C i2c i2c_int_flag_smbto
    clearInterruptFlagI2C i2c i2c_int_flag_ouerr
    clearInterruptFlagI2C i2c i2c_int_flag_lostarb
    clearInterruptFlagI2C i2c i2c_int_flag_berr
    clearInterruptFlagI2C i2c i2c_int_flag_pecerr
