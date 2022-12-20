{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.USART  where

import           Device.GD32F3x0.SystemClock
import           Feature
import qualified Interface                   as I
import qualified Interface.Counter           as I
import qualified Interface.USART             as I
import           Ivory.Language
import           Ivory.Stdlib


data USART = forall a. (I.USART a) => USART Int a

instance I.Interface USART where

  dependencies (USART n usart) = defMemArea     (buff'' n)
                               : defMemArea     (index'' n)
                               : defMemArea     (timestamp'' n)
                               : I.dependencies (I.HandleUSART usart (onReceive n) onDrain)


  initialize (USART n usart) = I.initialize (I.HandleUSART usart (onReceive n) onDrain) <> [
    proc ("usart_" <> show n <> "_init") $ body $ do
      I.setBaudrate   usart 1_000_000
      I.setWordLength usart I.WL_8b
      I.setParity     usart I.None
      I.enable        usart
    ]


instance Task USART where
  tasks (USART n usart) = [
    Step Nothing $ proc ("usart_" <> show n <> "_step") $ body $ do
      index <- deref $ index' n
      timestamp <- deref $ timestamp' n
      t <- I.readCounter systemClock
      when (index >? 0 .&& t - timestamp >? 400) $ do
        I.transmit usart (toCArray $ buff' n) index
        store (index' n) 0
    ]


onReceive n b = do
    index <- deref $ index' n
    let ix = toIx index
    store (buff' n ! ix) b
    store (index' n) (index + 1)
    store (timestamp' n) =<< I.readCounter systemClock

onDrain :: Ivory eff ()
onDrain = pure ()


timestamp'' :: Int -> MemArea (Stored Uint32)
timestamp'' n = area ("timestamp_" <> show n) $ Just (ival 0)

timestamp' :: Int -> Ref Global (Stored Uint32)
timestamp' = addrOf . timestamp''


index'' :: Int -> MemArea (Stored Uint16)
index'' n = area ("index_" <> show n) $ Just (ival 0)

index' :: Int -> Ref Global (Stored Uint16)
index' = addrOf . index''


buff'' :: Int -> MemArea (Array 512 (Stored Uint16))
buff'' n = area ("buff_" <> show n) Nothing

buff' :: Int -> Ref Global (Array 512 (Stored Uint16))
buff' = addrOf . buff''
