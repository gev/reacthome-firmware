{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Interface.RS485 where

import           Include
import           Initialize
import           Interface.GPIO
import qualified Interface.USART as I
import           Ivory.Language


data RS485 where
    RS485 :: (I.USART u, OUT o)
          => { n     :: Int
             , usart :: u
             , rede  :: o
             }
            -> RS485


data HandleRS485 r = HandleRS485
    { re         :: r
    , onReceive  :: Uint16 -> forall eff. Ivory eff ()
    , onTransmit :: forall eff. Ivory eff ()
    }

transmit :: RS485
         -> Ref r (CArray (Stored Uint16))
         -> Uint16
         -> Ivory (ProcEffects s ()) ()
transmit (RS485 {usart, rede}) buffer length =
    set rede >> I.transmit usart buffer length

setBaudrate :: RS485 -> Uint32 -> Ivory eff ()
setBaudrate (RS485 {usart}) = I.setBaudrate usart

setWordLength :: RS485 -> I.WordLength -> Ivory eff ()
setWordLength (RS485 {usart}) = I.setWordLength usart

setStopBit :: RS485 -> I.StopBit -> Ivory eff ()
setStopBit (RS485 {usart}) = I.setStopBit usart

setParity :: RS485 -> I.Parity -> Ivory eff ()
setParity (RS485 {usart}) = I.setParity usart


instance Include (HandleRS485 RS485) where
    include (HandleRS485 (RS485 {usart, rede}) onReceive onTransmit) = do
        include rede
        include $ I.HandleUSART usart onReceive onTransmit (reset rede)


instance Initialize RS485 where
    initialize (RS485 {n, usart, rede}) =
        initR : initialize rede <> initialize usart
        where initR =
                proc ("rs485_" <> show n <> "_init") $ body
                                                     $ reset rede
