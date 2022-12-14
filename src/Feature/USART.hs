{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}
{-# LANGUAGE FlexibleContexts #-}

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
                               : defMemArea (buff'' n)
                               : defMemArea (index'' n)
                               : I.dependencies usart
                               

  initialize (USART n usart) = I.initialize usart <> [
    proc ("usart_" <> show n <> "_init") $ body $ do
      I.setBaudrate   usart 1_000_000
      I.setWordLength usart I.WL_8b
      I.setParity     usart I.None
      I.enable        usart
    ]


t0'' :: Int -> MemArea (Stored Uint32)
t0'' n = area ("t0_" <> show n) $ Just (ival 0)

t0' :: Int -> Ref Global (Stored Uint32)
t0' = addrOf . t0''


index'' :: Int -> MemArea (Stored Uint16)
index'' n = area ("index_" <> show n) $ Just (ival 0)

index' :: Int -> Ref Global (Stored Uint16)
index' = addrOf . index''


buff'' :: Int -> MemArea (Array 512 (Stored Uint16))
buff'' n = area ("buff_" <> show n) $ Just (iarray [ival 0])

buff' :: Int -> Ref Global (Array 512 (Stored Uint16))
buff' = addrOf . buff''


instance Task USART where
  tasks (USART n usart) = [
    Step Nothing $ proc ("usart_" <> show n <> "_step") $ body $ do
      forever $ do
                hasReceived <- I.hasReceived usart
                ifte_ hasReceived 
                      ( do
                        index <- deref $ index' n
                        let ix = toIx index
                        store (buff' n ! ix) =<< I.receive usart
                        store (index' n) (index + 1)
                        store (t0' n) =<< I.readCounter systemClock
                      )
                      breakOut
      -- forever $ do 
      --           t1 <- I.readCounter systemClock
      --           t0 <- deref $ t0' n
      --           when ( t1 - t0 >? 400 ) breakOut
      index <- deref $ index' n
      let ix = toIx index
      for ix $ \i -> do
        forever $ do
          canTransmit <- I.canTransmit usart
          when canTransmit breakOut
        I.transmit usart =<< deref (buff' n ! i)
      store (index' n) 0

      -- ifte_ ( t1 - t0 <? 400 )
      --       ( forever $ do
      --           hasReceived <- I.hasReceived usart
      --           when hasReceived $ do
      --             b <- I.receive usart
      --             I.transmit usart b
      --             store (buff' n ! ix) b
      --             store (index' n) (index + 1)
      --             store (t0' n) t1
      --       )
      --       ( do
      --           for ix $ \i -> do
      --             forever $ do
      --               canTransmit <- I.canTransmit usart
      --               when canTransmit breakOut
      --             b <- deref (buff' n ! i)
      --             I.transmit usart b
      --           store (index' n) 0
      --       )
    ]
