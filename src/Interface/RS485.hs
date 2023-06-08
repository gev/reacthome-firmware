{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Interface.RS485
    ( module Interface.RS485
    , WordLength (..)
    , StopBit    (..)
    , Parity     (..)
    ) where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Domain           as D
import           Core.Handler
import           Interface.GPIO.Output
import           Interface.MCU
import           Interface.UART        as U
import           Ivory.Language



data RS485 where
    RS485 :: (UART u, Output o)
          => { uart  :: u
             , rede  :: o
             }
            -> RS485



data HandleRS485 r = HandleRS485
    { re         :: r
    , onReceive  :: Uint16 -> forall eff. Ivory eff ()
    , onTransmit :: forall eff. Ivory eff ()
    }



rs485 :: (MonadWriter Context m, MonadReader (D.Domain p t) m, UART u, Output o)
      => Int -> (p -> m u) -> (p -> m o) -> m RS485
rs485 n uart' rede' = do
    mcu' <- asks D.mcu
    uart <- uart' $ peripherals mcu'
    rede <- rede' $ peripherals mcu'

    let initRS485' :: Def ('[] ':-> ())
        initRS485' = proc ("rs485_" <> show n <> "_init") $ body
                                                          $ reset rede
    addInit initRS485'
    pure RS485 { uart, rede }


transmit :: RS485
         -> Ref r (CArray (Stored Uint16))
         -> Uint16
         -> Ivory (ProcEffects s ()) ()
transmit RS485{..} buffer length =
    set rede >> U.transmit uart buffer length


configureRS485 :: RS485 -> Uint32 -> WordLength -> StopBit -> Parity -> Ivory eff ()
configureRS485 RS485{..} = configUART uart



instance Handler HandleRS485 RS485 where
    addHandler (HandleRS485 RS485{..} onReceive onTransmit) = do
        addHandler $ HandleUART uart onReceive onTransmit (Just $ reset rede)



setREDE :: RS485 -> Ivory eff ()
setREDE RS485{..} = set rede

resetREDE :: RS485 -> Ivory eff ()
resetREDE RS485{..} = reset rede
