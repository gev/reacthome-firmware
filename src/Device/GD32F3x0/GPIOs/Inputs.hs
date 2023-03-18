{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}

module Device.GD32F3x0.GPIOs.Inputs where

import           Core.Context
import           Data.Record
import qualified Device.GD32F3x0.GPIO.Input as D
import           Device.GD32F3x0.GPIOs
import qualified Interface.GPIOs.Inputs     as I
import           Ivory.Language


data Inputs = Inputs
    { getInputs :: [D.Input]
    , runInputs :: RunRecords GPIOStruct
    }


instance I.Inputs Inputs where
  get a = runGPIO undefined $ runInputs a


instance I.MakeInputs D.Input Inputs where
    makeInputs name getInputs = do
        addStruct (Proxy :: Proxy GPIOStruct)
        let runInputs = runRecordsFromList name fromPort $ D.getInput <$> getInputs
        runInputs addArea
        pure Inputs { getInputs, runInputs }
