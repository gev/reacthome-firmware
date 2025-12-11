module Device.GD32F4xx.DAC where

import Control.Monad.State (MonadState)
import Core.Context
import Device.GD32F4xx.GPIO.Port
import Interface.DAC qualified as I
import Ivory.Language
import Support.Device.GD32F4xx.DAC
import Support.Device.GD32F4xx.GPIO (GPIO_PUPD, gpio_pupd_none)
import Support.Device.GD32F4xx.RCU
import Ivory.Support

newtype DAC = DAC {dac :: DAC_PERIPH}

mkDAC :: (MonadState Context m) => (GPIO_PUPD -> Port) -> DAC_PERIPH -> m DAC
mkDAC mkPin dac = do
    let dac' = DAC{dac}
    addInit ("dac_" <> symbol dac) $ initDAC dac'
    initPort (mkPin gpio_pupd_none)
    pure dac'

initDAC :: DAC -> Ivory eff ()
initDAC DAC{..} = do
    enablePeriphClock rcu_dac
    disableTriggerDAC dac
    configWaveModeDAC dac dac_wave_disable
    enableDAC dac

setData :: DAC -> Uint16 -> Ivory eff ()
setData DAC{..} d = do
    enableOutputBufferDAC dac
    setDataDAC dac dac_align_12b_r d
    disableOutputBufferDAC dac

instance I.DAC DAC where
    getResolution = const 12
    getRefVoltage = const 3.3
    setAnalog = setData
