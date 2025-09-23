module Device.GD32F4xx.I2S where

import Data.Buffer
import Data.Record
import GHC.TypeLits
import Ivory.Language
import qualified Ivory.Stdlib as S
import Support.Cast
import Support.Device.GD32F4xx.DMA
import Support.Device.GD32F4xx.IRQ
import Support.Device.GD32F4xx.SPI

dataExchangeDmaI2S :: (KnownNat n) => SPI_DMA_DEFINITION -> SPI_PERIPH -> Record DMA_SINGLE_PARAM_STRUCT -> DMA_PERIPH -> DMA_CHANNEL -> DMA_INT -> Buffer n Uint32 -> Ivory (ProcEffects s ()) ()
dataExchangeDmaI2S definition spiPer dmaParams dmaPer dmaCh intDma buff = do
    store (dmaParams ~> memory0_addr) =<< castArrayUint32ToUint32 (toCArray buff)
    initSingleDMA dmaPer dmaCh dmaParams
    enableChannelDMA dmaPer dmaCh
    enableSpiDma spiPer definition
    enableInterruptDMA dmaPer dmaCh intDma

handleI2S :: DMA_PERIPH -> DMA_CHANNEL -> DMA_INT_FLAG -> Ivory (ProcEffects s ()) () -> Ivory (ProcEffects s ()) ()
handleI2S dmaPer dmaCh intFlag callBack = do
    f <- getInterruptFlagDMA dmaPer dmaCh intFlag
    S.when f $ do
        clearInterruptFlagDMA dmaPer dmaCh intFlag
        callBack
