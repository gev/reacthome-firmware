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

  dependencies (USART n usart) = defMemArea (t0'' n)
                               : defMemArea (index'' n)
                               : I.dependencies usart

  initialize (USART n usart) = I.initialize usart <> [
    proc ("usart_" <> show n <> "_init") $ body $ do
      I.setBaudrate   usart 1_000_000
      I.setWordLength usart I.WL_8b
      I.setParity     usart I.None
      I.enable        usart
    ]


t0'' :: Int -> MemArea ('Stored Uint32)
t0'' n = area ("t0_" <> show n) $ Just (ival 0)

t0' :: Int -> Ref 'Global ('Stored Uint32)
t0' = addrOf . t0''


index'' :: Int -> MemArea ('Stored Uint16)
index'' n = area ("index_" <> show n) $ Just (ival 0)

index' :: Int -> Ref 'Global ('Stored Uint16)
index' = addrOf . index''

instance Task USART where
  tasks (USART n usart) = [
    Step Nothing $ proc ("usart_" <> show n <> "_step") $ body $ do
      buff <- local $ iarray [ival 0]
      index <- deref $ index' n
      let ix = toIx index :: Ix 512
      t0 <- deref $ t0' n
      t1 <- I.readCounter systemClock
      store (t0' n) t1
      ifte_ ( t1 - t0 <? 400 )
            ( do
                hasReceived <- I.hasReceived usart
                when hasReceived $ do
                  b <- I.receive usart
                  store (buff ! ix) b
                  store (index' n) (index + 1)
            )
            ( do
                for ix $ \i -> do
                  forever $ do
                    canTransmit <- I.canTransmit usart
                    when canTransmit breakOut
                  b <- deref (buff ! i)
                  I.transmit usart b
                store (index' n) 0
            )
    ]
