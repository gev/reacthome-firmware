{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Device.GD32F3x0.DAC where

import Control.Monad.State (MonadState)
import Core.Context
import Data.Buffer
import Data.Record
import Device.GD32F3x0.GPIO.Port
import qualified Interface.DAC as I
import Ivory.Language
import Support.Device.GD32F3x0.DAC
import Support.Device.GD32F3x0.GPIO hiding (mode, rcu)
import Support.Device.GD32F3x0.RCU

data DAC = DAC {}

mkDAC :: (MonadState Context m) => (GPIO_PUPD -> Port) -> m DAC
mkDAC mkPin = do
    let dac = DAC
    addInit "dac" $ initDAC dac
    initPort (mkPin gpio_pupd_none)
    pure dac

initDAC :: DAC -> Ivory eff ()
initDAC DAC{} = do
    enablePeriphClock rcu_dac
    deinitDAC
    disableTriggerDAC
    configWaveModeDAC dac_wave_disable
    enableDAC

setData :: DAC -> Uint16 -> Ivory eff ()
setData DAC{} d = do
    enableOutputBufferDAC
    setDataDAC dac_align_12b_r d
    disableOutputBufferDAC

instance I.DAC DAC where
    getResolution = const 12
    getRefVoltage = const 3.3
    setAnalog = setData
