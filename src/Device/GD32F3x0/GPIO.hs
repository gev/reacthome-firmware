{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}

module Device.GD32F3x0.GPIO where

import           Include
import           Initialize
import qualified Interface.GPIO               as I
import           Ivory.Language
import           Ivory.Language.Module
import           Support.Device.GD32F3x0.GPIO as S
import           Support.Device.GD32F3x0.RCU


newtype IN  = IN  PORT

newtype OUT = OUT PORT

data PORT = PORT
    { rcu  :: RCU_PERIPH
    , gpio :: GPIO_PERIPH
    , pin  :: GPIO_PIN
    , mode :: MODE
    }

data MODE
    = MF GPIO_MODE
    | AF GPIO_AF

in_pa_0   = input pa_0
in_pa_1   = input pa_1
in_pa_2   = input pa_2
in_pa_3   = input pa_3
in_pa_4   = input pa_4
in_pa_5   = input pa_5
in_pa_6   = input pa_6
in_pa_7   = input pa_7
in_pa_8   = input pa_8
in_pa_9   = input pa_9
in_pa_10  = input pa_10
in_pa_11  = input pa_11
in_pa_12  = input pa_12
in_pa_13  = input pa_13
in_pa_14  = input pa_14
in_pa_15  = input pa_15

out_pa_0  = output pa_0
out_pa_1  = output pa_1
out_pa_2  = output pa_2
out_pa_3  = output pa_3
out_pa_4  = output pa_4
out_pa_5  = output pa_5
out_pa_6  = output pa_6
out_pa_7  = output pa_7
out_pa_8  = output pa_8
out_pa_9  = output pa_9
out_pa_10  = output pa_10
out_pa_11 = output pa_11
out_pa_12 = output pa_12
out_pa_13 = output pa_13
out_pa_14 = output pa_14
out_pa_15 = output pa_15

pa_0  = pa GPIO_PIN_0
pa_1  = pa GPIO_PIN_1
pa_2  = pa GPIO_PIN_2
pa_3  = pa GPIO_PIN_3
pa_4  = pa GPIO_PIN_4
pa_5  = pa GPIO_PIN_5
pa_6  = pa GPIO_PIN_6
pa_7  = pa GPIO_PIN_7
pa_8  = pa GPIO_PIN_8
pa_9  = pa GPIO_PIN_9
pa_10  = pa GPIO_PIN_10
pa_11 = pa GPIO_PIN_11
pa_12  = pa GPIO_PIN_12
pa_13 = pa GPIO_PIN_13
pa_14  = pa GPIO_PIN_14
pa_15 = pa GPIO_PIN_15

pa :: GPIO_PIN -> MODE -> PORT
pa = PORT RCU_GPIOA GPIOA


input :: (MODE -> PORT) -> IN
input = IN . io GPIO_MODE_INPUT

output :: (MODE -> PORT) -> OUT
output = OUT . io GPIO_MODE_OUTPUT

io :: GPIO_MODE -> (MODE -> PORT) -> PORT
io m p = p $ MF m


instance Include IN where
    include = const include'

instance Initialize IN where
    initialize (IN p) = [initialize' p]


instance Include OUT where
    include = const include'

instance Initialize OUT where
    initialize (OUT p) = [initialize' p]


instance I.IN IN where
    get = undefined

instance I.OUT OUT where
    set   (OUT (PORT {gpio, pin})) = S.setBit gpio pin
    reset (OUT (PORT {gpio, pin})) = S.resetBit gpio pin


include' :: ModuleM ()
include' =    inclRCU >> inclGPIO

initialize' :: PORT -> Def ('[] ':-> ())
initialize' (PORT {rcu, gpio, pin, mode}) =
        proc (show gpio <> "_" <> show pin <>"_init") $ body $ do
            enablePeriphClock rcu
            setOutputOptions gpio GPIO_OTYPE_PP GPIO_OSPEED_50MHZ pin
            case mode of
                (MF mode) -> setMode gpio mode GPIO_PUPD_NONE pin
                (AF mode) -> setMode gpio GPIO_MODE_AF GPIO_PUPD_NONE pin
                          >> setAF gpio mode pin
