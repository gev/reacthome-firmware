{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Interface.RS485 where

import           Interface       as I
import           Interface.GPIO  as I
import           Interface.USART as I
import           Ivory.Language


data RS485 where
  RS485 :: (I.USART u, OUT o)
        => { usart :: u
           , rede  :: o
           }
        -> RS485


startReceive :: RS485 -> Ivory eff ()
startReceive (RS485 {rede}) = I.reset rede

receive :: RS485 -> Ivory eff Uint16
receive (RS485 {usart}) = I.receive usart

startTransmit :: RS485 -> Ivory eff ()
startTransmit (RS485 {rede}) = I.set rede

transmit :: RS485 -> Uint16 -> Ivory eff ()
transmit (RS485 {usart}) = I.transmit usart


setBaudrate :: RS485 -> Uint32 -> Ivory eff ()
setBaudrate (RS485 {usart}) = I.setBaudrate usart

setWordLength :: RS485 -> WordLength -> Ivory eff ()
setWordLength (RS485 {usart}) = I.setWordLength usart

setStopBit :: RS485 -> StopBit -> Ivory eff ()
setStopBit (RS485 {usart}) = I.setStopBit usart

setParity :: RS485 -> Parity -> Ivory eff ()
setParity (RS485 {usart}) = I.setParity usart

hasReceived :: RS485 -> Ivory eff IBool
hasReceived (RS485 {usart}) = I.hasReceived usart

hasTransmitted :: RS485 -> Ivory eff IBool
hasTransmitted (RS485 {usart}) = I.hasTransmitted usart

canTransmit :: RS485 -> Ivory eff IBool
canTransmit (RS485 {usart}) = I.canTransmit usart


instance Interface RS485 where

  dependencies (RS485 {usart, rede}) =
   I.dependencies usart <> I.dependencies rede

  initialize (RS485 {usart, rede}) =
   I.initialize usart <> I.initialize rede
