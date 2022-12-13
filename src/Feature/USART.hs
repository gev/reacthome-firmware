{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}

module Feature.USART  where

import           Device.GD32F3x0.SystemClock
import           Feature
import qualified Interface                   as I
import qualified Interface.Timer             as I
import qualified Interface.USART             as I
import           Ivory.Language
import           Ivory.Stdlib


data USART = forall a. (I.USART a) => USART Int a

instance I.Interface USART where

  dependencies (USART _ usart) = I.dependencies usart

  initialize (USART n usart) = I.initialize usart <> [
    proc ("usart_" <> show n <> "_init") $ body $ do
      I.setBaudrate   usart 1_000_000
      I.setWordLength usart I.WL_8b
      I.setParity     usart I.None
      I.enable        usart
    ]




instance Task USART where
  tasks (USART n usart) = [
    Step Nothing $ proc ("usart_" <> show n <> "_step") $ body $ do
      index <- local  (ival (0 :: Sint16))
      buff <- local $ iarray [ival 0]
      t0 <- I.readCounter systemClock
      forever $ do

        hasReceived <- I.hasReceived usart
        ifte_ hasReceived
              ( do
                  b <- I.receive usart
                  i <- deref index
                  let ix = toIx i :: Ix 512
                  store (buff ! ix) b
                  store index (i + 1)
              )
              breakOut
        -- when hasReceived $ do

        --   forever $ do
        --     canTransmit <- I.canTransmit usart
        --     when canTransmit breakOut
        --   I.transmit usart b
    ]
