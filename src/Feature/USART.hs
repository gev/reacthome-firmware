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



data USART = forall a. (I.USART a) => USART
  { name      :: String
  , u         :: a
  , timestamp :: Value      Uint32
  , sizeTx    :: Value      Sint32
  , buffTx    :: Buffer 512 Uint16
  , buffRx    :: Buffer 512 Uint16
  , queueRx   :: Queue  512
  }


usart n u = Feature $ USART
  { name      = name
  , u         = u
  , timestamp = value  ( name <> "_rx_timestamp" ) 0
  , sizeTx    = value  ( name <> "_tx_size" ) 0
  , buffTx    = buffer $ name <> "_tx"
  , buffRx    = buffer $ name <> "_rx"
  , queueRx   = queue  $ name <> "_rx"
  } where name = "usart_" <> show n


instance Include USART where
  include (USART {u, timestamp, sizeTx, buffTx, buffRx, queueRx}) = do
    include timestamp
    include sizeTx
    include buffTx
    include buffRx
    include queueRx
    include $ I.HandleUSART u (onReceive timestamp queueRx buffRx) onDrain


instance Initialize USART where
  initialize (USART {name, u}) =
    initialize u <> [
      proc (name <> "_init") $ body $ do
        I.setBaudrate   u 1_000_000
        I.setWordLength u I.WL_8b
        I.setParity     u I.None
        I.enable        u
    ]


instance Task USART where
  tasks (USART {name, u, timestamp, sizeTx, buffRx, buffTx, queueRx}) = [
    yeld name $ do
      t1 <- readCounter systemClock
      t0 <- getValue timestamp
      when (t1 - t0 >? 400) $ do
        size <- getValue sizeTx
        when (size >? 0) $ do
          let tx = getBuffer buffTx
          I.transmit u (toCArray tx) size
          setValue sizeTx 0
      pop queueRx $ \ix -> do
        size <- getValue sizeTx
        getItem buffRx ix >>= setItem buffTx (toIx size)
        setValue sizeTx $ size + 1
    ]


onReceive :: Value  Uint32
          -> Queue  512
          -> Buffer 512 Uint16
          -> Uint16
          -> Ivory  eff ()
onReceive timestamp queueRx buffRx b = do
    push queueRx $ \ix -> do
      setValue timestamp =<< readCounter systemClock
      setItem buffRx ix b


onDrain :: Ivory eff ()
onDrain = pure ()
