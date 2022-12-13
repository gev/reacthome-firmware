{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

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

  dependencies (USART _ usart) = defMemArea t0''
                               : defMemArea index''
                               : I.dependencies usart

  initialize (USART n usart) = I.initialize usart <> [
    proc ("usart_" <> show n <> "_init") $ body $ do
      I.setBaudrate   usart 1_000_000
      I.setWordLength usart I.WL_8b
      I.setParity     usart I.None
      I.enable        usart
    ]


t0'' :: MemArea ('Stored Uint32)
t0'' = area "t0" $ Just (ival 0)

t0' :: Ref 'Global ('Stored Uint32)
t0' = addrOf t0''


index'' :: MemArea ('Stored Uint16)
index'' = area "index" $ Just (ival 0)

index' = addrOf index''

instance Task USART where
  tasks (USART n usart) = [
    Step Nothing $ proc ("usart_" <> show n <> "_step") $ body $ do
      buff <- local $ iarray [ival 0]
      index <- deref index'
      let ix = toIx index :: Ix 512
      t0 <- deref t0'
      t1 <- I.readCounter systemClock
      store t0' t1
      ifte_ ( t1 - t0 <? 40 )
            ( do
                hasReceived <- I.hasReceived usart
                when hasReceived $ do
                  b <- I.receive usart
                  store (buff ! ix) b
                  store index' (index + 1)
            )
            ( do
                for ix $ \i -> do
                  forever $ do
                    canTransmit <- I.canTransmit usart
                    when canTransmit breakOut
                  b <- deref (buff ! i)
                  I.transmit usart b
                store index' 0
            )
    ]
