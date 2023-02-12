module Formula.Relay6 where

import           Data.Function   ((&))
import           Device.GD32F3x0
import           Feature.RBUS
import           Feature.Relays
import           Formula
import           Interface.RS485


relay6 :: Formula
relay6 = Formula { mcu      = gd32ffx0
                 , features = [ rbus   1 $ RS485 1 (gd32ffx0 & usart_1)
                                                   (gd32ffx0 & out_pa_4)
                              , relays [ gd32ffx0 & out_pb_0
                                       , gd32ffx0 & out_pa_5
                                       , gd32ffx0 & out_pa_6
                                       , gd32ffx0 & out_pa_0
                                       , gd32ffx0 & out_pa_7
                                       , gd32ffx0 & out_pa_1
                                       ]
                              ]
                 }
