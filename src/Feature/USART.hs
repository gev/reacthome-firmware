{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.USART  where

import           Device.GD32F3x0.SystemClock
import           Feature
import           Include
import           Initialize
import           Interface.Counter
import           Interface.RS485             (transmit)
import qualified Interface.USART             as I
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Buffer
import           Util.Queue

data USART = forall a. (I.USART a) => USART Int a

instance Include USART where
  include (USART n usart) = do
    defMemArea (timestamp'' n)
    include    (buffTx n)
    include    (buffRx n)
    include    (queueRx n)
    include    (I.HandleUSART usart (onReceive n) onDrain)


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
      let rx = queueRx n
      size <- size rx
      when (size >? 0) $ do
        timestamp <- deref $ timestamp' n
        t <- readCounter systemClock
        when (t - timestamp >? 400) $ do
          let tx = addrOf (buffTx n)
          for size $ \ix -> pop rx $ \jx -> do
            let rx = addrOf $ buffRx n
            store (tx!ix) =<<deref (rx!jx)
          I.transmit usart (toCArray tx)
                           (fromIx size)
    ]


onReceive n b = do
    push (queueRx n) $ \ix -> do
      let rx = addrOf $ buffRx n
      store (rx!ix) b
      store (timestamp' n) =<< readCounter systemClock

onDrain :: Ivory eff ()
onDrain = pure ()


timestamp'' :: Int -> MemArea (Stored Uint32)
timestamp'' n = area ("usart_" <> show n <> "_timestamp_rx") $ Just (ival 0)

timestamp' :: Int -> Ref Global (Stored Uint32)
timestamp' = addrOf . timestamp''


buffTx :: Int -> Buffer 512 Uint16
buffTx n = buffer $ "usart_" <> show n <> "_tx"

buffRx :: Int -> Buffer 512 Uint16
buffRx n = buffer $ "usart_" <> show n <> "_rx"

queueRx :: Int -> Queue 512
queueRx n = queue $ "usart_" <> show n <> "_rx"
