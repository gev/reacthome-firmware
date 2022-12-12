{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}

module Feature.USART  where

import           Feature
import qualified Interface       as I
import qualified Interface.USART as I
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
  task (USART n usart) =
    Step Nothing $ proc ("usart_" <> show n <> "_step") $ body $ do
      hasReceived <- I.hasReceived usart
      when hasReceived $ do
        b <- I.receive usart
        forever $ do
          canTransmit <- I.canTransmit usart
          when canTransmit breakOut
        I.transmit usart b
