{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.USART  where

import           Control.Monad               ((>=>))
import           Device.GD32F3x0.SystemClock
import           Feature
import           GHC.TypeNats
import           Include
import           Initialize
import           Interface.Counter
import qualified Interface.USART             as I
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Data.Buffer
import           Util.Data.Class
import           Util.Data.Concurrent.Queue
import           Util.Data.Value
import Support.Device.GD32F3x0.USART
import           Support.Device.GD32F3x0.GPIO  as G
import Support.CMSIS.CoreCM4



data USART = forall a. (I.USART a) => USART
  { name      :: String
  , u         :: a
  , timestamp :: Value      Uint32
  , sizeTx    :: Value      Sint32
  , lockTx    :: Value      IBool
  , buffTx    :: Buffer 512 Uint16
  , buffRx    :: Buffer 512 Uint16
  , queueRx   :: Queue  512
  }


usart :: I.USART u => Int -> u -> Feature
usart n u = Feature $ USART
  { name      = name
  , u         = u
  , timestamp = value  ( name <> "_rx_timestamp" ) 0
  , sizeTx    = value  ( name <> "_tx_size" ) 0
  , lockTx    = value  ( name <> "_tx_lock" ) false
  , buffTx    = buffer $ name <> "_tx"
  , buffRx    = buffer $ name <> "_rx"
  , queueRx   = queue  $ name <> "_rx"
  } where name = "usart_" <> show n


instance Include USART where
  include (USART {u, timestamp, sizeTx, lockTx, buffTx, buffRx, queueRx}) = do
    include timestamp
    include sizeTx
    include lockTx
    include buffTx
    include buffRx
    include queueRx
    include $ I.HandleUSART u (onReceive timestamp queueRx buffRx) onTransmit onDrain
    inclGPIO
    inclUSART
    inclCoreCM4


instance Initialize USART where
  initialize (USART {name, u}) =
    initialize u <> [
      proc (name <> "_init") $ body $ do
        pure ()
        -- I.setBaudrate   u 1_000_000
        -- I.setWordLength u I.WL_8b
        -- I.setParity     u I.None
        -- I.enable        u
    ]


instance Task USART where
  tasks (USART {name, u, timestamp, sizeTx, buffRx, buffTx, queueRx, lockTx}) = [
    yeld name $ do
      t0 <- getValue timestamp
      t1 <- readCounter systemClock
      when (t1 - t0 >? 400) $ do
        size <- getValue sizeTx
        when (size >? 0) $ do
          G.resetBit GPIOA GPIO_PIN_4
          setValue sizeTx 0
          -- setValue lockTx true
          let tx = getBuffer buffTx
          I.transmit u (toCArray tx) size
      pop queueRx $ \ix -> do
        size <- getValue sizeTx
        getItem buffRx ix >>= setItem buffTx (toIx size)
        setValue sizeTx $ size + 1

    ]



onReceive timestamp queueRx buffRx b = do
    push queueRx $ \ix -> do
      setItem buffRx ix b
      setValue timestamp =<< readCounter systemClock
      G.setBit GPIOA GPIO_PIN_4


onTransmit :: Ivory eff ()
onTransmit = pure ()


onDrain :: Ivory eff ()
onDrain = pure ()
