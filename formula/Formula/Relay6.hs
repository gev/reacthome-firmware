module Formula.Relay6 where

import           Core.Formula
import           Data.Function   ((&))
import           Device.GD32F3x0
import           Feature.RBUS
import           Feature.Relays
import           Interface.RS485

relay6 :: Formula
relay6 = Formula { model    = 0xa0
                 , version  = (2, 1)
                 , mcu      = gd32f3x0
                 , features = [ rbus   1 $ rs485 1 usart_1 out_pa_4
                              , relays [ out_pb_0
                                       , out_pa_5
                                       , out_pa_6
                                       , out_pa_0
                                       , out_pa_7
                                       , out_pa_1
                                       ]
                              ]
                 }
