{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}

module Feature.Dopplers where


import           Control.Monad        (zipWithM_)
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Interface.MCU        (MCU, peripherals, systemClock)
import           Ivory.Language
import           Ivory.Stdlib

import           Core.Context
import qualified Core.Domain          as D
import qualified Core.Transport       as T
import           Data.Buffer
import           Data.Index
import qualified Interface.ADC        as I
import Data.Value
import Core.Task (delay)
import Ivory.Language.Uint (Uint8(Uint8))
import Core.Actions


data Dopplers = forall a. I.ADC a => Dopplers
    { adc         ::  a
    , index       ::  Value Uint8
    , buff        ::  Buffer 129 Uint8
    , transmit    ::  forall s t. Buffer 129 Uint8 -> Ivory (ProcEffects s t) ()
    }


dopplers :: ( MonadState Context m
            , MonadReader (D.Domain p t c) m
            , T.Transport t
            , I.ADC a
            ) => (p -> m a) -> m Dopplers
dopplers analogInput = do
    mcu              <- asks D.mcu
    let clock        = systemClock mcu
    transport        <- asks D.transport
    let peripherals' = peripherals mcu
    adc              <- analogInput peripherals'
    buff             <- values "doppler_tx_buffer" $ actionDopplerRaw : replicate 128 0
    index            <- value  "doppler_index"  1

    let dopp = Dopplers { adc
                        , index
                        , buff
                        , transmit = T.transmitBuffer transport
                        }

    -- addInit "doppler_init" $ 
    --     store (buff ! 0) actionDopplerRaw

    addTask $ delay   1 "doppler_measure"  $ measure dopp
    addTask $ delay 128 "doppler_send_raw" $ sendRAW dopp

    pure dopp


measure :: Dopplers -> Ivory (ProcEffects s ()) ()
measure Dopplers {..} = do
    index' <- deref index
    store (buff ! toIx index') . castDefault . (`iDiv` 16) =<< I.getAnalog adc
    ifte_ (index' <? 128) 
          (store index $ index' + 1)
          (store index 1)


sendRAW :: Dopplers -> Ivory (ProcEffects s ()) ()
sendRAW Dopplers {..} = transmit buff


