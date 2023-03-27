module Formula.Echo where

import           Core.Formula
import           Device.GD32F4xx
import           Feature.Echo         as E
import           Ivory.Language
import           Transport.USART.RBUS


echo :: Formula GD32F4xx
echo = Formula { name       = "echo"
               , model      = 0xff
               , version    = (1, 0)
               , shouldInit = false
               , transport  = rbus usart_1
               , features   = [ E.echo ]
               }
