{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Interface.RS485 where

import           Control.Monad.Reader
import           Core.Context
import           Interface.GPIO.Output
import           Interface.MCU
import qualified Interface.USART       as I
import           Ivory.Language


data RS485 where
    RS485 :: (I.USART u, Output o)
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


rs485 :: (I.USART u, Output o)
      => Int
      -> (p -> u)
      -> (p -> o)
      -> Reader p RS485
rs485 n usart rede = do
    peripherals <- ask
    pure $ RS485 { n     = n
                 , usart = usart peripherals
                 , rede  = rede  peripherals
                 }


transmit :: RS485
         -> Ref r (CArray (Stored Uint16))
         -> Uint16
         -> Ivory (ProcEffects s ()) ()
transmit (RS485 {..}) buffer length =
    set rede >> I.transmit usart buffer length

setBaudrate :: RS485 -> Uint32 -> Ivory eff ()
setBaudrate (RS485 {..}) = I.setBaudrate usart

setWordLength :: RS485 -> I.WordLength -> Ivory eff ()
setWordLength (RS485 {..}) = I.setWordLength usart

setStopBit :: RS485 -> I.StopBit -> Ivory eff ()
setStopBit (RS485 {..}) = I.setStopBit usart

setParity :: RS485 -> I.Parity -> Ivory eff ()
setParity (RS485 {..}) = I.setParity usart


instance Include (HandleRS485 RS485) where
    include (HandleRS485 (RS485 {..}) onReceive onTransmit) = do
        include $ I.HandleUSART usart onReceive onTransmit (reset rede)


instance Include RS485 where
    include (RS485 {..}) = do
        include rede
        include usart
        include initRS485'
            where
                initRS485' :: Def ('[] ':-> ())
                initRS485' = proc ("rs485_" <> show n <> "_init") $ body
                                                                  $ reset rede
