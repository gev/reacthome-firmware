{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RankNTypes       #-}

module Device.GD32F3x0.GPIOs where

import           Core.Include
import           Core.Initialize
import           Data.Foldable
import           Data.Record
import qualified Device.GD32F3x0.GPIO          as D
import           GHC.TypeLits
import qualified Interface.GPIOs               as I
import           Ivory.Language
import           Ivory.Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.GPIO



type GPIOStruct = "gpio_struct"

[ivory|
    struct gpio_struct
    { gpio :: Uint32
    ; pin  :: Uint32
    }
|]



data Inputs = Inputs
    { getInputs :: [D.Input]
    , runInputs :: RunRecords GPIOStruct
    }

data Outputs = Outputs
    { getOutputs :: [D.Output]
    , runOutputs :: RunRecords GPIOStruct
    }



inputs :: String -> [D.Input] -> Inputs
inputs name os = Inputs
    { getInputs  = os
    , runInputs = runRecords name convert $ D.getInput <$> os
    }

outputs :: String -> [D.Output] -> Outputs
outputs name os = Outputs
    { getOutputs  = os
    , runOutputs = runRecords name convert $ D.getOutput <$> os
    }

convert :: ConvertRecord D.Port GPIOStruct
convert p = [gpio .=  get D.gpio, pin .= get D.pin]
    where get f = ival . def $ f p



instance Include Inputs where
    include a = do
        defStruct (Proxy :: Proxy GPIOStruct)
        traverse_ include $ getInputs a

instance Initialize Inputs where
    initialize = concatMap initialize . getInputs

instance I.Inputs Inputs where
  get a = runGPIO undefined $ runInputs a



instance Include Outputs where
    include a = do
        defStruct (Proxy :: Proxy GPIOStruct)
        traverse_ include $ getOutputs a

instance Initialize Outputs where
    initialize = concatMap initialize . getOutputs

instance I.Outputs Outputs where
  reset a = runGPIO (call_ gpio_bit_reset) $ runOutputs a
  set a = runGPIO (call_ gpio_bit_set) $ runOutputs a



runGPIO :: (Uint32 -> Uint32 -> Ivory eff a)
        -> RunRecords GPIOStruct
        -> (forall n. KnownNat n => Ix n) -> Ivory eff a
runGPIO f run i = run go
    where
        go o = do
            let o' = addrOf o
            g <- deref $ o' ! i ~> gpio
            p <- deref $ o' ! i ~> pin
            f g p
