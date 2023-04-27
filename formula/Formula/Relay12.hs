module Formula.Relay12 where

import           Core.Formula
import           Device.GD32F3x0
import           Feature.Relays       (relays)
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS


relay12 :: Formula GD32F3x0
relay12 = Formula { name       = "relay12"
                  , model      = 0xae
                  , version    = (3, 0)
                  , shouldInit = true
                  , transport  = rbus $ rs485 1 uart_0 out_pb_2
                  , features   = [ relays [ out_pb_1
                                          , out_pa_11
                                          , out_pa_8
                                          , out_pb_0
                                          , out_pa_9
                                          , out_pa_10
                                          , out_pa_0
                                          , out_pa_1
                                          , out_pa_2
                                          , out_pa_3
                                          , out_pa_6
                                          , out_pa_7
                                          ]
                               ]
                  }
