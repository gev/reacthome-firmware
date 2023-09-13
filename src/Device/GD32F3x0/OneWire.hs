{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}

module Device.GD32F3x0.OneWire where

import           Control.Monad.State
import           Core.Context
import           Core.Handler
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Device.GD32F3x0.GPIO.Mode
import           Device.GD32F3x0.GPIO.OpenDrain
import           Device.GD32F3x0.GPIO.Port
import           Device.GD32F3x0.Timer          (Timer)
import qualified Interface.OneWire              as I
import           Ivory.Language
import qualified Interface.Timer                as I
import           Support.CMSIS.CoreCM4
import qualified Interface.GPIO.OpenDrain       as I



data OneWire = OneWire
    { port  :: OpenDrain
    , timer :: Timer
    , tmpB  :: Buffer 32 Uint8
    , tmpQ  :: Queue  32
    , tmpV  :: Value     Uint8
    , count :: Value     Uint8
    }


mkOneWire :: MonadState Context m
         => (Uint32 -> Uint32 -> m Timer)
         -> m OpenDrain
         -> m OneWire
mkOneWire cfg od = do
    port  <- od
    timer <- cfg 1_000_000 1
    tmpB  <- buffer "one_wire_tmp"
    tmpQ  <- queue  "one_wire_tmp"
    tmpV  <- value_ "one_wire_tmp_value"
    count <- value  "one_wire_count" 0
    let handlerOW = do
                    I.set port
                    nop 10
                    I.reset port
    addHandler $ I.HandleTimer timer handlerOW
    pure $ OneWire { port, timer, tmpB, tmpQ, tmpV, count }



instance I.OneWire OneWire where
    read _ = pure 0

    write OneWire {tmpB, tmpQ} v =
        push tmpQ $ \ix -> store (tmpB ! toIx ix) v



instance Handler I.HandleOneWire OneWire where
  addHandler _ = pure ()
