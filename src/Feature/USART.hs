{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}
{-# LANGUAGE FlexibleContexts   #-}

module Feature.USART  where

import           Control.Monad               ((>=>))
import           Device.GD32F3x0.SystemClock
import           Feature
import           Include
import           Initialize
import           Interface.Counter
import           Interface.RS485             (transmit)
import qualified Interface.USART             as I
import           Ivory.Language
import           Ivory.Stdlib
-- import           Util.Data.Buffer
import           Util.Data.Concurrent.Queue
import           Util.Data.DataBuffer
import           Util.Data.DataValue
-- import           Util.Data.Value

data USART = forall a. (I.USART a) => USART Int a

instance Include USART where
  include (USART n usart) = do
    include (timestamp n)
    include (buffTx n)
    include (buffRx n)
    include (queueRx n)
    include (I.HandleUSART usart (onReceive n) onDrain)


instance Initialize USART where
  initialize (USART n usart) =
    initialize usart <> [
      proc ("usart_" <> show n <> "_init") $ body $ do
        I.setBaudrate   usart 1_000_000
        I.setWordLength usart I.WL_8b
        I.setParity     usart I.None
        I.enable        usart
    ]


instance Task USART where
  tasks (USART n usart) = [
    Step Nothing $ proc ("usart_" <> show n <> "_step") $ body $ do
      size <- size $ queueRx n
      when (size >? 0) $ do
        ts <- loadValue (timestamp n)
        t <- readCounter systemClock
        when (t - ts >? 400) $ do
          for size $ \ix -> pop (queueRx n) $
            loadItem (buffRx n) >=> storeItem (buffTx n) ix
          processBuffer (buffTx n) $ \tx ->
            I.transmit usart (toCArray tx)
                             (fromIx size)
    ]
onReceive :: Int -> Uint16 -> Ivory eff ()
onReceive n b = do
    push (queueRx n) $ \ix -> do
      storeValue (timestamp n) =<< readCounter systemClock
      storeItem (buffRx n) ix b

onDrain :: Ivory eff ()
onDrain = pure ()


timestamp :: Int -> DataValue Uint32
timestamp n = justValue ("usart_" <> show n <> "_timestamp_rx") 0


buffTx :: Int -> DataBuffer 512 Uint16
buffTx n = buffer $ "usart_" <> show n <> "_tx"

buffRx :: Int -> DataBuffer 512 Uint16
buffRx n = buffer $ "usart_" <> show n <> "_rx"

queueRx :: Int -> Queue 512
queueRx n = queue $ "usart_" <> show n <> "_rx"
