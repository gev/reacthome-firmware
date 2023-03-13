{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Interface.RS485 where

import           Control.Monad.Reader
import           Core.Include
import           Core.Initialize
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


rs485 :: (I.USART u, Output o, MCU mcu)
      => Int
      -> (mcu -> u)
      -> (mcu -> o)
      -> Reader mcu RS485
rs485 n usart rede = do
    mcu <- ask
    pure $ RS485 { n     = n
                 , usart = usart mcu
                 , rede  = rede  mcu
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
        include rede
        include $ I.HandleUSART usart onReceive onTransmit (reset rede)


instance Initialize RS485 where
    initialize (RS485 {..}) =
        initialize rede <> initialize usart <> [initR]
        where initR =
                proc ("rs485_" <> show n <> "_init") $ body
                                                     $ reset rede
