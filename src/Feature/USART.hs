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
import qualified Interface.USART             as I
import           Ivory.Language
import           Ivory.Stdlib


data USART = forall a. (I.USART a) => USART Int a

instance Include USART where
  include (USART n usart) = do
    defMemArea (buff'' n)
    defMemArea (index'' n)
    defMemArea (timestamp'' n)
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
      index <- deref $ index' n
      when (index >? 0) $ do
        timestamp <- deref $ timestamp' n
        t <- readCounter systemClock
        when (t - timestamp >? 400) $ do
          I.transmit usart (toCArray $ buff' n) index
          store (index' n) 0
    ]


onReceive n b = do
    index <- deref $ index' n
    let ix = toIx index
    store (buff' n ! ix) b
    store (index' n) (index + 1)
    store (timestamp' n) =<< readCounter systemClock

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
