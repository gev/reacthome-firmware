{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}

module Interface.RS485 where

import           Interface       as I
import           Interface.GPIO  as I
import qualified Interface.USART as I
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

transmit :: RS485 -> Ref r (CArray (Stored Uint16)) -> Uint16 -> Ivory (ProcEffects s ()) ()
transmit (RS485 {usart}) = I.transmit usart


setBaudrate :: RS485 -> Uint32 -> Ivory eff ()
setBaudrate (RS485 {usart}) = I.setBaudrate usart

setWordLength :: RS485 -> I.WordLength -> Ivory eff ()
setWordLength (RS485 {usart}) = I.setWordLength usart

setStopBit :: RS485 -> I.StopBit -> Ivory eff ()
setStopBit (RS485 {usart}) = I.setStopBit usart

setParity :: RS485 -> I.Parity -> Ivory eff ()
setParity (RS485 {usart}) = I.setParity usart


instance Interface (I.HandleUSART RS485) where

  include (I.HandleUSART (RS485 usart rede) onReceive onDrain) =
   I.include (I.HandleUSART usart onReceive onDrain) <> I.include rede

  initialize (I.HandleUSART (RS485 usart rede) onReceive onDrain) =
   I.initialize (I.HandleUSART usart onReceive onDrain) <> I.initialize rede
