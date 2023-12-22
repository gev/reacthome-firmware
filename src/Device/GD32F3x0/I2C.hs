{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Device.GD32F3x0.I2C where

import           Control.Monad.State
import           Core.Context
import           Device.GD32F3x0.GPIO.Port    hiding (rcu)
import qualified Interface.I2C                as I
import           Ivory.Support
import           Support.Device.GD32F3x0.I2C
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU

data I2C = I2C
    { i2c      :: I2C_PERIPH
    , eventIrq :: IRQn
    , errorIrq :: IRQn
    }


mkI2C :: MonadState Context m
       => I2C_PERIPH
       -> RCU_PERIPH
       -> IRQn
       -> IRQn
       -> Port
       -> Port
       -> m I2C
mkI2C i2c rcu eventIrq errorIrq sda scl = do

    initPort sda
    initPort scl
    
    addInit (symbol i2c) $ do
        enablePeriphClock rcu
        configClockI2C i2c 400000 i2c_dtcy_2
        configModeAddrI2C i2c i2c_i2cmode_enable i2c_addformat_7bits 0
        enableI2C i2c 
        -- configAckI2C i2c i2c_ack_enable
        enableIrqNvic       eventIrq 0 0
        enableIrqNvic       errorIrq 0 0

    pure I2C { i2c, eventIrq, errorIrq }





