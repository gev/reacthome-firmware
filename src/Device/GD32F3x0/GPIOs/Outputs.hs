{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Device.GD32F3x0.GPIOs.Outputs where

import           Core.Context
import           Data.Record
import qualified Device.GD32F3x0.GPIO.Output  as D
import           Device.GD32F3x0.GPIOs
import qualified Interface.GPIOs.Outputs      as I
import           Ivory.Language
import           Support.Device.GD32F3x0.GPIO



data Outputs = Outputs
    { getOutputs :: [D.Output]
    , runOutputs :: RunRecords GPIOStruct
    }



instance I.Outputs Outputs where
    reset a = runGPIO (call_ gpio_bit_reset) $ runOutputs a
    set a = runGPIO (call_ gpio_bit_set) $ runOutputs a

instance I.MakeOutputs D.Output Outputs where
    makeOutputs name getOutputs = do
        let runOutputs = runRecordsFromList name fromPort $ D.getOutput <$> getOutputs
        mapM_ include getOutputs
        runOutputs include
        pure Outputs { getOutputs, runOutputs }
