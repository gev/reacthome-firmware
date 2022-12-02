{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Feature.USART  where

import           Feature
import qualified Interface       as I
import qualified Interface.USART as I
import           Ivory.Language
import           Ivory.Stdlib


data USART a = (I.USART a) => USART Int a

instance Prepare (USART b) where
  prepare (USART n usart) =
     Pack (I.dependencies usart)
          [initialize' n usart]
          [step' n usart]

initialize' :: I.USART u => Int -> u -> Def ('[] ':-> ())
initialize' n usart =
  proc ("usart_" <> show n <> "_init") $ body $ do
    I.initialize    usart
    I.setBaudrate   usart 1_000_000
    I.setWordLength usart I.WL_8b
    I.setParity     usart I.None

step' :: I.USART u => Int -> u -> Def ('[] ':-> ())
step' n usart =
  proc ("usart_" <> show n <> "_step") $ body $ do
    hasReceived <- I.hasReceived usart
    when hasReceived $ do
      b <- I.receive usart
      forever $ do
        isBusy <- I.hasTransmitted usart
        unless isBusy breakOut
        I.transmit usart b
