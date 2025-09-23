{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Interface.OneWire where

import Control.Monad.State (MonadState)
import Core.Context
import Core.Handler
import qualified Interface.GPIO.OpenDrain as OD
import qualified Interface.Timer as T
import Ivory.Language

data OneWire where
  OneWire ::
    (OD.OpenDrain od, T.Timer t) =>
    { port :: od
    , timer :: t
    } ->
    OneWire

mkOneWire ::
  (MonadState Context m, OD.OpenDrain od, T.Timer t) =>
  (Uint32 -> Uint32 -> m t) ->
  m od ->
  m OneWire
mkOneWire cfg od = do
  port <- od
  timer <- cfg 1_000_000 10
  let onewire = OneWire{port, timer}
  addInit "onewire" $ initOneWire onewire
  pure onewire

initOneWire :: OneWire -> Ivory eff ()
initOneWire OneWire{..} = OD.set port

pullUp :: OneWire -> Ivory eff ()
pullUp OneWire{..} = OD.set port

pullDown :: OneWire -> Ivory eff ()
pullDown OneWire{..} = OD.reset port

getState :: OneWire -> Ivory eff IBool
getState OneWire{..} = OD.get port

enableOneWire :: OneWire -> Ivory eff ()
enableOneWire OneWire{..} = T.enableInterrupt timer

disableOneWire :: OneWire -> Ivory eff ()
disableOneWire OneWire{..} = T.disableInterrupt timer

handleTimer :: (MonadState Context m) => OneWire -> (forall eff. Ivory eff ()) -> m ()
handleTimer OneWire{..} handler =
  addHandler $
    T.HandleTimer timer handler
