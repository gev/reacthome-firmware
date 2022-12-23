{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.USART  where

import           Control.Monad               ((>=>))
import           Device.GD32F3x0.SystemClock
import           Feature
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
  { n         :: Int
    , u       :: a
  , timestamp :: Value      Uint32
  , buffTx    :: Buffer 512 Uint16
  , buffRx    :: Buffer 512 Uint16
  , queueRx   :: Queue  512
  }


usart n u = Feature $ USART
  { n = n
  , u = u
  , timestamp = value  ( name n "timestamp" ) 0
  , buffTx    = buffer $ name n "tx"
  , buffRx    = buffer $ name n "rx"
  , queueRx   = queue  $ name n "rx"
  }


instance Include USART where
  include usart@(USART {u, timestamp, buffTx, buffRx, queueRx}) = do
    include timestamp
    include buffTx
    include buffRx
    include queueRx
    include $ I.HandleUSART u (onReceive usart) onDrain


instance Initialize USART where
  initialize (USART {n, u}) =
    initialize u <> [
      proc (name n "init") $ body $ do
        I.setBaudrate   u 1_000_000
        I.setWordLength u I.WL_8b
        I.setParity     u I.None
        I.enable        u
    ]


instance Task USART where
  tasks (USART {n, u, timestamp, buffRx, buffTx, queueRx}) = [
    Step Nothing $ proc (name n "step") $ body $ do
      size <- size queueRx
      when (size >? 0) $ do
        ts <- getValue timestamp
        t <- readCounter systemClock
        when (t - ts >? 400) $ do
          for size $ \ix -> pop queueRx $
            getItem buffRx >=> setItem buffTx ix
          process buffTx $ \tx ->
            I.transmit u (toCArray tx)
                         (fromIx size)
    ]


onReceive :: USART -> Uint16 -> Ivory eff ()
onReceive (USART {n, timestamp, queueRx, buffRx}) b = do
    push queueRx $ \ix -> do
      setValue timestamp =<< readCounter systemClock
      setItem buffRx ix b


onDrain :: Ivory eff ()
onDrain = pure ()


name :: Int -> String -> String
name n id = "usart_" <> show n <> "_" <> id
