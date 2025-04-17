{-# LANGUAGE NumericUnderscores #-}

module Formula.Doppler5 where

import           Control.Monad.Reader   (MonadReader)
import           Control.Monad.State    (MonadState)
import           Core.Context
import           Core.Domain            (Domain)
import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.DInputs
import           Feature.Dopplers
import           Feature.ALED           (ALED, aled)
import           Implementation.Doppler (doppler)
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS

doppler5 :: Formula GD32F3x0
doppler5 = Formula { name            = "doppler5"
                   , model           = deviceTypeDoppler5Di4
                   , version         = (2, 6)
                   , shouldInit      = true
                   , mcu             = gd32f330k8u6
                   , quartzFrequency =  8_000_000
                   , systemFrequency = 84_000_000
                   , implementation  = doppler (rbus $ rs485 uart_1 out_pa_4)
                                               (dopplers $  adc_pa_1
                                                         :> adc_pa_6
                                                         :> adc_pa_5
                                                         :> adc_pa_0
                                                         :> adc_pa_7
                                                         :> Nil
                                               )
                                               (dinputs  $  in_pa_12
                                                         :> in_pa_11
                                                         :> in_pa_10
                                                         :> in_pa_9
                                                         :> Nil
                                                )
                                                (aled npx_pwm_0 etc)
                   }
