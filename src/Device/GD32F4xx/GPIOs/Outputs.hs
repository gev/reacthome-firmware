{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Device.GD32F4xx.GPIOs.Outputs where

import           Core.Include
import           Core.Initialize
import           Data.Foldable
import           Data.Record
import qualified Device.GD32F4xx.GPIO.Output  as D
import           Device.GD32F4xx.GPIOs
import qualified Interface.GPIOs.Outputs      as I
import           Ivory.Language
import           Support.Device.GD32F4xx.GPIO



data Outputs = Outputs
    { getOutputs :: [D.Output]
    , runOutputs :: RunRecords GPIOStruct
    }



instance Include Outputs where
    include (Outputs {..}) = runOutputs include

instance Initialize Outputs where
    initialize = concatMap initialize . getOutputs

instance I.Outputs Outputs where
    reset a = runGPIO (call_ gpio_bit_reset) $ runOutputs a
    set a = runGPIO (call_ gpio_bit_set) $ runOutputs a

instance I.MakeOutputs D.Output Outputs where
    makeOutputs name os = Outputs { getOutputs = os
                                  , runOutputs = runRecordsFromList name fromPort $ D.getOutput <$> os
                                  }
