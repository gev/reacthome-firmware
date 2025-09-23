
module Device.GD32F4xx.GPIO.Port where

import Control.Monad.State (MonadState)
import Core.Context
import Device.GD32F4xx.GPIO.Mode
import Ivory.Language
import Ivory.Support (ExtSymbol (symbol))
import Support.Device.GD32F4xx.GPIO
import Support.Device.GD32F4xx.RCU

data Port = Port
    { rcu :: RCU_PERIPH
    , gpio :: GPIO_PERIPH
    , pin :: GPIO_PIN
    , mode :: Mode
    , pupd :: GPIO_PUPD
    }

initPort :: (MonadState Context m) => Port -> m (Def ('[] ':-> ()))
initPort p@Port{..} = addInit (show p) $ do
    enablePeriphClock rcu
    case mode of
        (MF mode otype) -> initMode mode otype
        (AF mode) ->
            initMode gpio_mode_af gpio_otype_pp
                >> setAF gpio mode pin
  where
    initMode mode otype = do
        setOutputOptions gpio otype gpio_ospeed_50mhz pin
        setMode gpio mode pupd pin

instance Show Port where
    show Port{..} = symbol gpio <> "_" <> symbol pin
