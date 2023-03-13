{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Device.GD32F3x0.GPIOs.Inputs where

import           Core.Include
import           Core.Initialize
import           Data.Foldable
import           Data.Record
import qualified Device.GD32F3x0.GPIO.Input as D
import           Device.GD32F3x0.GPIOs
import qualified Interface.GPIOs.Inputs     as I
import           Ivory.Language



data Inputs = Inputs
    { getInputs :: [D.Input]
    , runInputs :: RunRecords GPIOStruct
    }



instance Include Inputs where
    include (Inputs {..}) = do
        traverse_ include getInputs
        runInputs include

instance Initialize Inputs where
    initialize = concatMap initialize . getInputs

instance I.Inputs Inputs where
  get a = runGPIO undefined $ runInputs a

instance I.MakeInputs D.Input Inputs where
    makeInputs name is = Inputs { getInputs = is
                                , runInputs = runRecordsFromList name fromPort $ D.getInput <$> is
                                }
