module Formula.MixA6x6 where

import           Core.Formula
import           Device.GD32F3x0
import           Feature.MixA
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS


mixA6x6 :: Formula GD32F3x0
mixA6x6 = Formula { name       = "mixA6x6"
                  , model      = 0xff
                  , version    = (1, 0)
                  , shouldInit = false
                  , transport  = rbus $ rs485 1 uart_0 out_pb_2
                  , features   = [ mixA [ in_pa_4
                                        , in_pa_15
                                        , in_pb_3
                                        , in_pb_4
                                        , in_pb_5
                                        , in_pa_12
                                        , in_pa_9
                                        , in_pa_10
                                        , in_pa_8
                                        , in_pb_0
                                        , in_pb_1
                                        , in_pa_11
                                        ]
                                        [ out_pa_0
                                        , out_pa_1
                                        , out_pa_2
                                        , out_pa_3
                                        , out_pa_6
                                        , out_pa_7
                                        ]
                                 ]
                  }
