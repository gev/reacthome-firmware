{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}



module Device.GD32F4xx.UART where

import qualified Control.Monad                 as M
import           Control.Monad.State           (MonadState)
import           Core.Context
import           Core.Handler
import           Data.Buffer
import           Data.Maybe
import           Data.Record
import           Data.Value
import           Device.GD32F4xx.GPIO.Port
import           GHC.TypeNats
import qualified Interface.UART                as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support
import           Ivory.Support.Device.GD32F4xx
import           Support.Cast
import           Support.Device.GD32F4xx
import           Support.Device.GD32F4xx.GPIO
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.Misc
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.USART as S


data UART n = UART
    { uart    :: USART_PERIPH
    , rcu     :: RCU_PERIPH
    , uartIRQ :: IRQn
    , rx      :: Port
    , tx      :: Port
    , txBuff  :: Buffer n Uint16
    , index   :: Value Uint16
    , size    :: Value Uint16
    }


mkUART :: (MonadState Context m, KnownNat n)
       => USART_PERIPH
       -> RCU_PERIPH
       -> IRQn
       -> (GPIO_PUPD -> Port)
       -> (GPIO_PUPD -> Port)
       -> m (UART n)
mkUART uart rcu uartIRQ rx' tx' = do

    txBuff    <- buffer (symbol uart <> "_tx_buff")
    index     <- value_ (symbol uart <> "_tx_index")
    size      <- value_ (symbol uart <> "_tx_size")

    let rx = rx' gpio_pupd_none
    let tx = tx' gpio_pupd_none

    initPort rx
    initPort tx

    addInit (symbol uart) $ do
            enableIrqNvic       uartIRQ 0 0
            enablePeriphClock   rcu

    pure UART { uart, rcu, uartIRQ, rx, tx, txBuff, index, size }



instance KnownNat n => Handler I.HandleUART (UART n) where
    addHandler (I.HandleUART u@UART{..} onReceive onTransmit onDrain) = do
        addModule $ makeIRQHandler uartIRQ (handleUART u onReceive onTransmit onDrain)



handleUART :: KnownNat n => UART n -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Maybe (Ivory eff ()) -> Ivory eff ()
handleUART u@UART{..} onReceive onTransmit onDrain = do
    handleReceive uart onReceive
    handleTransmit u onTransmit onDrain
    mapM_ (handleDrain uart) onDrain


handleTransmit :: KnownNat n => UART n -> Ivory eff () -> Maybe (Ivory eff ()) -> Ivory eff ()
handleTransmit UART{..} onTransmit onDrain = do
    tbe <- getInterruptFlag uart usart_int_flag_tbe
    when tbe $ do
        clearInterruptFlag uart usart_int_flag_tbe
        index' <- deref index
        size'  <- deref size
        ifte_ (safeCast index' <? size')
            (do
                transmitData uart =<< deref (txBuff ! toIx index')
                store index $ index' + 1
            )
            (do
                disableInterrupt uart usart_int_tbe
                M.when (isJust onDrain) $ do
                    enableInterrupt uart usart_int_tc
                onTransmit
            )


handleReceive :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
handleReceive uart onReceive = do
    rbne <- getInterruptFlag    uart usart_int_flag_rbne
    when rbne $ do
        clearInterruptFlag     uart usart_int_flag_rbne
        ferr        <- getFlag uart usart_flag_ferr
        nerr        <- getFlag uart usart_flag_nerr
        orerr       <- getFlag uart usart_flag_orerr
        perr        <- getFlag uart usart_flag_perr
        clearFlag              uart usart_flag_ferr
        clearFlag              uart usart_flag_nerr
        clearFlag              uart usart_flag_orerr
        clearFlag              uart usart_flag_perr
        when (iNot $ ferr .|| nerr .|| orerr .|| perr) $ do
            value <- S.receiveData uart
            onReceive value


handleDrain :: USART_PERIPH -> Ivory eff () -> Ivory eff ()
handleDrain uart onDrain = do
    tc <- getInterruptFlag      uart usart_int_flag_tc
    when tc $ do
        clearInterruptFlag      uart usart_int_flag_tc
        disableInterrupt        uart usart_int_tc
        onDrain



instance KnownNat n => I.UART (UART n) where
    configUART (UART {..}) baudrate length stop parity = do
        deinitUSART         uart
        configReceive       uart usart_receive_enable
        configTransmit      uart usart_transmit_enable
        enableInterrupt     uart usart_int_rbne
        setBaudrate         uart baudrate
        setWordLength       uart $ coerceWordLength length
        setStopBit          uart $ coerceStopBit    stop
        configParity        uart $ coerceParity     parity
        enableUSART         uart


    transmit UART{..} write = do
        store size 0
        store index 0
        write $ \value -> do
            size' <- deref size
            store (txBuff ! toIx size') value
            store size $ size' + 1
        enableInterrupt uart usart_int_tbe


    enable u = enableUSART (uart u)


coerceWordLength :: I.WordLength -> USART_WORD_LENGTH
coerceWordLength I.WL_8b = usart_wl_8bit
coerceWordLength I.WL_9b = usart_wl_9bit

coerceStopBit :: I.StopBit -> USART_STOP_BIT
coerceStopBit I.SB_0_5b = usart_stb_0_5bit
coerceStopBit I.SB_1b   = usart_stb_1bit
coerceStopBit I.SB_1_5b = usart_stb_1_5bit
coerceStopBit I.SB_2b   = usart_stb_2bit

coerceParity :: I.Parity -> USART_PARITY_CFG
coerceParity I.None = usart_pm_none
coerceParity I.Even = usart_pm_even
coerceParity I.Odd  = usart_pm_odd



instance Show (UART n) where
    show UART{..} = symbol uart
