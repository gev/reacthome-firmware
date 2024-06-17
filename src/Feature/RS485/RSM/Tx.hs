{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.RS485.RSM.Tx where

import           Control.Monad                 (zipWithM_)
import           Data.Buffer
import           Data.Concurrent.Queue
import           Feature.RS485.RSM.Data
import           GHC.TypeNats
import           Interface.Mac
import qualified Interface.RS485               as RS
import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Stdlib




txHandle :: RSM -> Ivory eff ()
txHandle RSM{..} = do
    store rxLock false
    store txLock false



rsTransmit :: RSM -> Uint16 -> Ivory (ProcEffects s t) ()
rsTransmit RSM{..} size = do
    let array = toCArray txBuff
    RS.transmit rs array size
    store txLock true

