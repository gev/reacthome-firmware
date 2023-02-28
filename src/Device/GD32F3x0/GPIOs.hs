{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

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
    { getInputs = os
    , runInputs = runRecords name convert $ D.getInput <$> os
    }

outputs :: String -> [D.Output] -> Outputs
outputs name os = Outputs
    { getOutputs = os
    , runOutputs = runRecords name convert $ D.getOutput <$> os
    }

convert :: ConvertRecord D.Port GPIOStruct
convert p = [ gpio .= ival (def $ D.gpio p)
            , pin  .= ival (def $ D.pin  p)
            ]



instance KnownNat n => Include (Records n GPIOStruct) where
    include r = do
        defStruct (Proxy :: Proxy GPIOStruct)
        defMemArea r



instance Include Inputs where
    include (Inputs {getInputs, runInputs}) = do
        traverse_ include getInputs
        runInputs include

instance Initialize Inputs where
    initialize = concatMap initialize . getInputs

instance I.Inputs Inputs where
  get a = runGPIO undefined $ runInputs a



instance Include Outputs where
    include (Outputs {getOutputs, runOutputs}) = do
        traverse_ include getOutputs
        runOutputs include

instance Initialize Outputs where
    initialize = concatMap initialize . getOutputs

instance I.Outputs Outputs where
  reset a = runGPIO (call_ gpio_bit_reset) $ runOutputs a
  set a = runGPIO (call_ gpio_bit_set) $ runOutputs a



runGPIO :: (Uint32 -> Uint32 -> Ivory eff a)
        -> RunRecords GPIOStruct
        -> (forall n. KnownNat n => Ix n) -> Ivory eff a
runGPIO f run ix = run $ \o -> do
    let o' = addrOf o
    gpio' <- deref (o' ! ix ~> gpio)
    pin'  <- deref (o' ! ix ~> pin )
    f gpio' pin'
