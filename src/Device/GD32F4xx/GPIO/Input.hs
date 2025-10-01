module Device.GD32F4xx.GPIO.Input where

import Control.Monad.State
import Core.Context
import Device.GD32F4xx.GPIO.Mode
import Device.GD32F4xx.GPIO.Port
import Interface.GPIO.Input qualified as I
import Ivory.Language
import Support.Device.GD32F4xx.GPIO as S

newtype Input = Input {getInput :: Port}

mkInput ::
    (MonadState Context m) =>
    (Mode -> GPIO_PUPD -> Port) ->
    GPIO_PUPD ->
    m Input
mkInput p pupd = do
    let port = p input pupd
    initPort port
    pure $ Input port

instance I.Input Input where
    get (Input Port{..}) = S.getInputBit gpio pin
