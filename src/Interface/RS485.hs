{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Interface.RS485 where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Domain           as D
import           Core.Handler
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



rs485 :: (MonadWriter Context m, MonadReader (D.Domain p t) m, I.USART u, Output o)
      => Int -> (p -> m u) -> (p -> m o) -> m RS485
rs485 n usart' rede' = do
    mcu'        <- asks D.mcu
    usart       <- usart' $ peripherals mcu'
    rede        <- rede'  $ peripherals mcu'

    let initRS485' :: Def ('[] ':-> ())
        initRS485' = proc ("rs485_" <> show n <> "_init") $ body
                                                          $ reset rede
    addInit initRS485'
    pure RS485 { n, usart, rede }



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



instance Handler (HandleRS485 RS485) where
    addHandler (HandleRS485 (RS485 {..}) onReceive onTransmit) = do
        addHandler $ I.HandleUSART usart onReceive onTransmit (reset rede)
