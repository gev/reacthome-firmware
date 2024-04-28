{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
module Formula.Doppler5 where

import           Control.Monad.Reader   (MonadReader)
import           Control.Monad.State    (MonadState)
import           Core.Context
import           Core.Domain            (Domain)
import           Core.Formula
import           Core.Models
import           Device.GD32F3x0
import           Feature.DInputs
import           Feature.Dopplers
import           Implementation.Doppler hiding (dinputs, dopplers)
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS

doppler5 :: Formula GD32F3x0
doppler5 = Formula { name           = "doppler5"
                    , model          = deviceTypeDoppler5Di4
                    , version        = (1, 0)
                    , shouldInit     = false
                    , implementation = doppler (rbus $ rs485 uart_1 out_pa_4)
                                               (dopplers [ adc_pa_1
                                                         , adc_pa_6
                                                         , adc_pa_5
                                                         , adc_pa_0
                                                         , adc_pa_7
                                                         ]
                                               )
                                               (dinputs  [ in_pa_12
                                                         , in_pa_11
                                                         , in_pa_10
                                                         , in_pa_9
                                                         ]
                                               ) :: (MonadState Context m, MonadReader (Domain GD32F3x0 (Doppler 5)) m) => m (Doppler 5)
                    }
