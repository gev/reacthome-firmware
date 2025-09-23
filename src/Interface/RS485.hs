{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Interface.RS485 (
    module Interface.RS485,
    WordLength (..),
    StopBit (..),
    Parity (..),
) where

import Control.Monad.Reader
import Control.Monad.State
import Core.Context
import Core.Domain as D
import Core.Handler
import Data.Buffer
import GHC.TypeNats
import Interface.GPIO.Output
import Interface.GPIO.Port
import Interface.MCU
import Interface.UART as U
import Ivory.Language
import Ivory.Support

data RS485 rn tn where
    RS485 ::
        (UART (u rn tn), Output o, KnownNat rn, KnownNat tn) =>
        { uart :: u rn tn
        , rede :: o
        } ->
        RS485 rn tn

data HandleRS485 r = HandleRS485
    { re :: r
    , onReceive :: forall eff. Ivory eff ()
    , onTransmit :: forall eff. Ivory eff ()
    , onError :: forall eff. Ivory eff ()
    }

rs485 ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , UART (u rn tn)
    , Show (u rn tn)
    , Output o
    , Pull p d
    , KnownNat rn
    , KnownNat tn
    ) =>
    (p -> m (u rn tn)) ->
    (p -> d -> m o) ->
    m (RS485 rn tn)
rs485 uart' rede' = do
    mcu' <- asks D.mcu
    let peripherals' = peripherals mcu'
    uart <- uart' peripherals'
    rede <- rede' peripherals' (pullNone peripherals')

    addInit ("rs485_" <> show uart) $ reset rede

    pure RS485{uart, rede}

clearRX :: RS485 rn tn -> Ivory eff ()
clearRX RS485{..} = U.clearRX uart

receive ::
    RS485 rn tn ->
    (Uint16 -> Ivory (ProcEffects s t) ()) ->
    Ivory (ProcEffects s t) ()
receive RS485{..} = U.receive uart

transmit ::
    RS485 rn tn ->
    ((Uint16 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s t) ()) ->
    Ivory (ProcEffects s t) ()
transmit RS485{..} write =
    set rede >> U.transmit uart write

configureRS485 ::
    RS485 rn tn ->
    Uint32 ->
    WordLength ->
    StopBit ->
    Parity ->
    Ivory eff ()
configureRS485 RS485{..} = configUART uart

instance Handler HandleRS485 (RS485 rn tn) where
    addHandler (HandleRS485 RS485{..} onReceive onTransmit onError) = do
        addHandler $
            HandleUART
                uart
                onReceive
                onTransmit
                (Just $ reset rede)
                onError

setREDE :: RS485 rn tn -> Ivory eff ()
setREDE RS485{..} = set rede

resetREDE :: RS485 rn tn -> Ivory eff ()
resetREDE RS485{..} = reset rede
