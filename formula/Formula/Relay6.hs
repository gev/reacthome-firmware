module Formula.Relay6 where

import           Core.Formula
import           Device.GD32F3x0
import           Feature.Relays  (relays)
import           Interface.RS485
import           Ivory.Language
import           Transport.RBUS

relay6 :: Formula
relay6 = Formula { model      = 0xae
                 , version    = (3, 0)
                 , mcu        = gd32f3x0
                 , shouldInit = true
                 , transport  = rbus $ rs485 1 usart_1 out_pa_4
                 , features   = [ relays [ out_pb_0
                                         , out_pa_5
                                         , out_pa_6
                                         , out_pa_0
                                         , out_pa_7
                                         , out_pa_1
                                         , out_pb_2
                                         , out_pa_8
                                         , out_pa_9
                                         , out_pa_10
                                         , out_pa_11
                                         , out_pa_12
                                         ]
                              ]
                 }
