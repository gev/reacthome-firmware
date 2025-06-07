{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}



module Device.GD32F3x0.UART where

import qualified Control.Monad                 as M
import           Control.Monad.State           (MonadState)
import           Core.Context
import           Core.Handler
import           Core.Task
import           Data.Buffer
import           Data.Maybe
import           Data.Queue                    as Q
import           Data.Record
import           Data.Value
import           Device.GD32F3x0.GPIO.Port
import           GHC.TypeNats
import qualified Interface.UART                as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0
import           Support.Cast
import           Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.USART as S



data UART rn tn = UART
    { uart    :: USART_PERIPH
    , rcu     :: RCU_PERIPH
    , uartIRQ :: IRQn
    , rxQueue :: Queue  rn
    , rxBuff  :: Buffer rn Uint16
    , txBuff  :: Buffer tn Uint16
    , index   :: Value Uint16
    , size    :: Value Uint16
    }


mkUART :: (MonadState Context m, KnownNat rn, KnownNat tn)
       => USART_PERIPH
       -> RCU_PERIPH
       -> IRQn
       -> (GPIO_PUPD -> Port)
       -> (GPIO_PUPD -> Port)
       -> m (UART rn tn)
mkUART uart rcu uartIRQ rx' tx' = do

    rxQueue   <- queue  $ symbol uart <> "_rx"
    rxBuff    <- buffer $ symbol uart <> "_rx"
    txBuff    <- buffer $ symbol uart <> "_tx"
    index     <- value_ $ symbol uart <> "_tx_index"
    size      <- value_ $ symbol uart <> "_tx_size"

    let rx = rx' gpio_pupd_none
    let tx = tx' gpio_pupd_none

    initPort rx
    initPort tx

    addInit (symbol uart) $ do
            enableIrqNvic       uartIRQ 0 0
            enablePeriphClock   rcu

    pure UART { uart, rcu, uartIRQ, rxQueue, rxBuff, txBuff, index, size }



instance (KnownNat rn, KnownNat tn) => Handler I.HandleUART (UART rn tn) where
    addHandler (I.HandleUART u@UART{..} onReceive onTransmit onDrain onError) = do
        addModule $ makeIRQHandler uartIRQ (handleUART u onReceive onTransmit onDrain onError)



handleUART :: (KnownNat tn, KnownNat rn)
           => UART rn tn
           -> Ivory eff ()
           -> Ivory eff ()
           -> Maybe (Ivory eff ())
           -> Ivory eff ()
           -> Ivory eff ()
handleUART u@UART{..} onReceive onTransmit onDrain onError = do
    handleError u onError
    handleReceive u onReceive
    handleTransmit u onTransmit onDrain
    mapM_ (handleDrain uart) onDrain


handleTransmit :: KnownNat tn => UART rn tn -> Ivory eff () -> Maybe (Ivory eff ()) -> Ivory eff ()
handleTransmit UART{..} onTransmit onDrain = do
    tbe <- getInterruptFlag uart usart_int_flag_tbe
    when tbe $ do
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
        clearInterruptFlag  uart usart_int_flag_tbe


handleError :: KnownNat rn => UART rn tn -> Ivory eff () -> Ivory eff ()
handleError UART{..} onError = do
    errs <- sequence [
                       clear usart_int_flag_err_ferr   usart_flag_ferr
                     , clear usart_int_flag_err_nerr   usart_flag_nerr
                     , clear usart_int_flag_err_orerr  usart_flag_orerr
                     , clear usart_int_flag_perr       usart_flag_perr
                     , clear usart_int_flag_rbne_orerr usart_flag_orerr
                     ]
    let err = foldr (.||) false errs
    when err onError
    where clear i f = do
            i' <- getInterruptFlag uart i
            when i' $ do
                clearFlag uart f
                clearInterruptFlag uart i
            pure i'


handleReceive :: KnownNat rn => UART rn tn -> Ivory eff () -> Ivory eff ()
handleReceive UART{..} onReceive = do
    rbne <- getInterruptFlag   uart usart_int_flag_rbne
    when rbne $ do
        push rxQueue $ \i -> do
            store (rxBuff ! toIx i) =<< S.receiveData uart
            onReceive
        clearInterruptFlag     uart usart_int_flag_rbne


handleDrain :: USART_PERIPH -> Ivory eff () -> Ivory eff ()
handleDrain uart onDrain = do
    tc <- getInterruptFlag uart usart_int_flag_tc
    when tc $ do
        disableInterrupt   uart usart_int_tc
        onDrain
        clearInterruptFlag uart usart_int_flag_tc



instance (KnownNat rn, KnownNat tn) => I.UART (UART rn tn) where
    configUART (UART {..}) baudrate length stop parity = do
        deinitUSART     uart
        configReceive   uart usart_receive_enable
        configTransmit  uart usart_transmit_enable
        enableInterrupt uart usart_int_rbne
        enableInterrupt uart usart_int_err
        enableInterrupt uart usart_int_perr
        setBaudrate     uart baudrate
        setWordLength   uart $ coerceWordLength length
        setStopBit      uart $ coerceStopBit    stop
        configParity    uart $ coerceParity     parity
        enableUSART     uart

    clearRX UART{..} = Q.clear rxQueue

    receive UART{..} read =
        pop rxQueue $ \i ->
            read =<< deref (rxBuff ! toIx i)

    transmit UART{..} write = do
        store size 0
        write $ \value -> do
            size' <- deref size
            store (txBuff ! toIx size') value
            store size $ size' + 1
        store index 0
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



instance Show (UART rn tn) where
    show UART{..} = symbol uart
