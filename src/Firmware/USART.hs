module Firmware.USART  where

import           Device.GD32F3x0.USART (usart_1)
import           Feature
import           Feature.USART



usart :: [Feature]
usart =
  [ Feature $ USART 1 usart_1
  ]
