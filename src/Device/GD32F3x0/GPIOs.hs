{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}

module Device.GD32F3x0.GPIOs where

import           Core.Include
import           Core.Initialize
import qualified Interface.GPIO               as I
import           Ivory.Language
import           Ivory.Language.Module
import           Support.Device.GD32F3x0.GPIO as S
import           Support.Device.GD32F3x0.RCU



type GPIOStruct = "gpio_struct"

[ivory|
    struct gpio_struct
    { gpio :: Uint32
    ; pin  :: Uint32
    }
|]
