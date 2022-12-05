{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Interface.RS485 where

import           Interface.GPIO  as I
import           Interface.USART as I
import           Ivory.Language


data RS485 where
  RS485 :: (I.USART u, OUT o)
        => { usart :: u
           , rede  :: o
           }
        -> RS485


receive :: RS485 -> Ivory eff Uint16
receive (RS485 {usart}) = I.receive usart
