module Formula.Relay6 where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.Mac
import           Device.GD32F3x0.SystemClock
import           Device.GD32F3x0.USART
import           Feature.RBUS
import           Feature.Relays
import           Formula
import           Interface.RS485


relay6 :: Formula
relay6 = Formula { mac      = systemMac
                 , clock    = systemClock
                 , features = [ rbus   1 $ RS485 1 usart_1 out_pa_4
                              , relays [ out_pb_0
                                       , out_pa_5
                                       , out_pa_6
                                       , out_pa_0
                                       , out_pa_7
                                       , out_pa_1
                                       ]
                              ]
                 }
