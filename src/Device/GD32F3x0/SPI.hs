module Device.GD32F3x0.SPI where

import Control.Monad.State (MonadState)
import Core.Context
import Device.GD32F3x0.GPIO.Port
import Interface.SPI qualified as I
import Ivory.Language
import Ivory.Support
import Support.Device.GD32F3x0.GPIO
import Support.Device.GD32F3x0.RCU
import Support.Device.GD32F3x0.SPI

newtype SPI = SPI
    { spi :: SPI_PERIPH
    }

mkSPI ::
    (MonadState Context m) =>
    SPI_PERIPH ->
    RCU_PERIPH ->
    (GPIO_PUPD -> Port) ->
    (GPIO_PUPD -> Port) ->
    (GPIO_PUPD -> Port) ->
    m SPI
mkSPI spi rcu mosi' clk' nss' = do
    let mosi = mosi' gpio_pupd_none
    let clk = clk' gpio_pupd_none
    let nss = nss' gpio_pupd_none

    initPort mosi
    initPort clk
    initPort nss

    addInit (symbol spi) do
        enablePeriphClock rcu
        deinitSPII2S spi
        initSPI spi =<< local param
        enableSpiNssOutput spi
        enableSpiNsspMode spi
        enableSPI spi

    pure SPI{spi}

param =
    spiParam
        [ device_mode .= ival spi_master
        , trans_mode .= ival spi_transmode_fullduplex
        , frame_size .= ival spi_framesize_16bit
        , nss .= ival spi_nss_hard
        , endian .= ival spi_endian_msb
        , clock_polarity_phase .= ival spi_ck_pl_high_ph_1edge
        , prescale .= ival spi_psc_2
        ]

instance I.SPI SPI where
    transmit SPI{..} = transmitSPII2S spi
