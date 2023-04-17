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
                  , transport  = rbus $ rs485 1 uart_1 out_pa_4
                  , features   = [ mixA [ in_pb_0
                                        , in_pa_5
                                        , in_pa_6
                                        , in_pa_0
                                        , in_pa_7
                                        , in_pa_1
                                        ]
                                        [ out_pb_2
                                        , out_pa_8
                                        , out_pa_9
                                        , out_pa_10
                                        , out_pa_11
                                        , out_pa_12
                                        ]
                                 ]
                  }
