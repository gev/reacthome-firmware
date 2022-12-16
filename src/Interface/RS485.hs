{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Interface.RS485 where

import           Interface       as I
import           Interface.GPIO  as I
import           Interface.USART as I
import           Ivory.Language


data RS485 where
  RS485 :: (I.USART u, OUT o)
        => { usart :: I.OnReceive -> u
           , rede  :: o
           , onReceive :: I.OnReceive
           }
        -> RS485


startReceive :: RS485 -> Ivory eff ()
startReceive (RS485 {rede}) = I.reset rede

receive :: RS485 -> Ivory eff Uint16
receive (RS485 {usart, onReceive}) = I.receive $ usart onReceive

startTransmit :: RS485 -> Ivory eff ()
startTransmit (RS485 {rede}) = I.set rede

transmit :: RS485 -> Uint16 -> Ivory eff ()
transmit (RS485 {usart, onReceive}) = I.transmit $ usart onReceive


setBaudrate :: RS485 -> Uint32 -> Ivory eff ()
setBaudrate (RS485 {usart, onReceive}) = I.setBaudrate $ usart onReceive

setWordLength :: RS485 -> WordLength -> Ivory eff ()
setWordLength (RS485 {usart, onReceive}) = I.setWordLength $ usart onReceive

setStopBit :: RS485 -> StopBit -> Ivory eff ()
setStopBit (RS485 {usart, onReceive}) = I.setStopBit $ usart onReceive

setParity :: RS485 -> Parity -> Ivory eff ()
setParity (RS485 {usart, onReceive}) = I.setParity $ usart onReceive
hasReceived :: RS485 -> Ivory eff IBool
hasReceived (RS485 {usart, onReceive}) = I.hasReceived $ usart onReceive

hasTransmitted :: RS485 -> Ivory eff IBool
hasTransmitted (RS485 {usart, onReceive}) = I.hasTransmitted $ usart onReceive

canTransmit :: RS485 -> Ivory eff IBool
canTransmit (RS485 {usart, onReceive}) = I.canTransmit $ usart onReceive


instance Interface RS485 where

  dependencies (RS485 {usart, rede, onReceive}) =
   I.dependencies (usart onReceive) <> I.dependencies rede

  initialize (RS485 {usart, rede, onReceive}) =
   I.initialize (usart onReceive) <> I.initialize rede
