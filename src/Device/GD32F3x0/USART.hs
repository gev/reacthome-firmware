{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}

module Device.GD32F3x0.USART where

import           Device.GD32F3x0.GPIO
import           Include
import           Initialize
import qualified Interface.USART               as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support.Device.GD32F3x0
import           Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.GPIO  (GPIO_AF (GPIO_AF_1))
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.USART as S
import           Support.Util


data USART = USART
  { usart    :: USART_PERIPH
  , rcu      :: RCU_PERIPH
  , usartIRQ :: IRQn
  , dma      :: DMA_CHANNEL
  , dmaIRQ   :: IRQn
  , rx       :: PORT
  , tx       :: PORT
  }

usart_1 = USART USART1
                RCU_USART1
                USART1_IRQn
                DMA_CH3
                DMA_Channel3_4_IRQn
                (pa_3 $ AF GPIO_AF_1)
                (pa_2 $ AF GPIO_AF_1)


instance Include (I.HandleUSART USART) where
  include (I.HandleUSART (USART {usart, dma}) onReceive onTransmit onDrain) =
    inclG >> inclMisc >> inclUSART >> inclDMA >> inclUtil >> include' >>
    makeIRQHandler usart (handleIRQ usart onReceive onDrain) >>
    {-
      TODO: Add DMA IRQ handler
    -}
    incl (dmaIRQHandler dma usart onTransmit)



dmaIRQHandler :: DMA_CHANNEL -> USART_PERIPH -> (forall eff. Ivory eff ()) -> Def ('[] ':-> ())
dmaIRQHandler dma usart onTransmit = proc "DMA_Channel3_4_IRQHandler" $ body $ do
  f <- getInterruptFlagDMA dma DMA_INT_FLAG_FTF
  when f $ do
    clearInterruptFlagDMA dma DMA_INT_FLAG_G
    --disableInterrupt usart USART_INT_RBNE
    --enableInterrupt  usart USART_INT_TC
    onTransmit

instance Initialize USART where
  initialize (USART usart rcu usartIRQ dma dmaIRQ rx tx) =
    initialize' rx : initialize' tx : [
      proc (show usart <> "_init") $ body $ do
        enablePeriphClock RCU_DMA
        enableIrqNvic     usartIRQ 0 0
        enableIrqNvic     dmaIRQ   0 0
        enablePeriphClock rcu
        deinitUSART     usart
        configReceive   usart USART_RECEIVE_ENABLE
        configTransmit  usart USART_TRANSMIT_ENABLE
        setBaudrate     usart 1_000_000
        setWordLength   usart USART_WL_8BIT
        configParity    usart USART_PM_NONE
        enableInterrupt usart USART_INT_RBNE
        enableUSART     usart
        enableInterruptDMA dma DMA_INT_FTF
    ]


handleIRQ :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
handleIRQ usart onReceive onDrain = do
  rbne <- getInterruptFlag usart USART_INT_FLAG_RBNE
  when rbne $ onReceive =<< S.receiveData usart
  tc <- getInterruptFlag usart USART_INT_FLAG_TC
  when tc $ do
    clearInterruptFlag usart USART_INT_FLAG_TC
    --disableInterrupt usart USART_INT_TC
    --enableInterrupt  usart USART_INT_RBNE
    onDrain


instance I.USART USART where

  {-
    TODO: Should we "deinit" USART before change a configuration?
  -}
  setBaudrate   u     = S.setBaudrate $ usart u
  setWordLength u wl  = S.setWordLength (usart u) (coerceWordLength wl)
  setStopBit    u sb  = S.setStopBit    (usart u) (coerceStopBit sb)
  setParity     u p   = S.configParity  (usart u) (coerceParity p)

  receive = S.receiveData . usart

  transmit (USART {usart, dma}) buff n = do
    deinitDMA dma
    p <- tdata (def usart)
    m <- castArrayToUint32 buff
    initDMA dma dmaInitParam { dmaPeriphAddr = p
                             , dmaMemoryAddr = m
                             , dmaNumber     = n
                             }
    disableCirculationDMA dma
    disableMemoryToMemoryDMA dma
    --enableInterruptDMA dma DMA_INT_FTF
    enableChannelDMA dma
    transmitDMA usart USART_DENT_ENABLE


  enable         u  = enableUSART (usart u)


{-
  TODO: add all values of word length, stopbit and parity
-}
coerceWordLength :: I.WordLength -> USART_WORD_LENGTH
coerceWordLength I.WL_8b = USART_WL_8BIT

coerceStopBit :: I.StopBit -> USART_STOP_BIT
coerceStopBit I.SB_1b = USART_STB_1BIT

coerceParity :: I.Parity -> USART_PARITY_CFG
coerceParity I.None = USART_PM_NONE


dmaInitParam :: DMA_PARAM
dmaInitParam = dmaParam { dmaDirection    = DMA_MEMORY_TO_PERIPHERAL
                        , dmaMemoryInc    = DMA_MEMORY_INCREASE_ENABLE
                        , dmaMemoryWidth  = DMA_MEMORY_WIDTH_16BIT
                        , dmaPeriphInc    = DMA_PERIPH_INCREASE_DISABLE
                        , dmaPeriphWidth  = DMA_PERIPHERAL_WIDTH_16BIT
                        , dmaPriority     = DMA_PRIORITY_ULTRA_HIGH
                        }







{-
#include <stdio.h>
#include "gd32f3x0.h"
#include "gd32f350r_eval.h"

#define USART0_RDATA_ADDRESS      ((uint32_t)0x40013824)
#define USART0_TDATA_ADDRESS      ((uint32_t)0x40013828)
#define ARRAYNUM(arr_nanme)      (uint32_t)(sizeof(arr_nanme) / sizeof(*(arr_nanme)))

__IO FlagStatus g_transfer_complete = RESET;
uint8_t rxbuffer[10];
uint8_t txbuffer[] = "\n\rUSART DMA interrupt receive and transmit example, please input 10 bytes:\n\r";

void usart0_gpio_config(void);
void usart0_config(void);
void nvic_config(void);

/*!
    \brief      main function
    \param[in]  none
    \param[out] none
    \retval     none
*/
int main(void)
{
    dma_parameter_struct dma_init_struct;
    /* enable DMA clock */
    rcu_periph_clock_enable(RCU_DMA);
    /* initilize the com */
    usart0_gpio_config();
    usart0_config();
    /*configure DMA interrupt*/
    nvic_config();

    /* initialize DMA channel1 */
    dma_deinit(DMA_CH1);
    dma_struct_para_init(&dma_init_struct);

    dma_init_struct.direction = DMA_MEMORY_TO_PERIPHERAL;
    dma_init_struct.memory_addr = (uint32_t)txbuffer;
    dma_init_struct.memory_inc = DMA_MEMORY_INCREASE_ENABLE;
    dma_init_struct.memory_width = DMA_MEMORY_WIDTH_8BIT;
    dma_init_struct.number = ARRAYNUM(txbuffer);
    dma_init_struct.periph_addr = (uint32_t) &USART_TDATA(USART0);
    dma_init_struct.periph_inc = DMA_PERIPH_INCREASE_DISABLE;
    dma_init_struct.periph_width = DMA_PERIPHERAL_WIDTH_8BIT;
    dma_init_struct.priority = DMA_PRIORITY_ULTRA_HIGH;
    dma_init(DMA_CH1, &dma_init_struct);

    /* initialize DMA channel2 */
    dma_deinit(DMA_CH2);
    dma_init_struct.direction = DMA_PERIPHERAL_TO_MEMORY;
    dma_init_struct.memory_addr = (uint32_t)rxbuffer;
    dma_init_struct.memory_inc = DMA_MEMORY_INCREASE_ENABLE;
    dma_init_struct.memory_width = DMA_MEMORY_WIDTH_8BIT;
    dma_init_struct.number = 10;
    dma_init_struct.periph_addr = (uint32_t) &USART_RDATA(USART0);
    dma_init_struct.periph_inc = DMA_PERIPH_INCREASE_DISABLE;
    dma_init_struct.memory_width = DMA_PERIPHERAL_WIDTH_8BIT;
    dma_init_struct.priority = DMA_PRIORITY_ULTRA_HIGH;
    dma_init(DMA_CH2, &dma_init_struct);

    /* configure DMA mode */
    dma_circulation_disable(DMA_CH1);
    dma_memory_to_memory_disable(DMA_CH1);
    dma_circulation_disable(DMA_CH2);
    dma_memory_to_memory_disable(DMA_CH2);

    /* USART DMA enable for reception */
    usart_dma_receive_config(USART0, USART_DENR_ENABLE);
    /* enable DMA channel2 transfer complete interrupt */
    dma_interrupt_enable(DMA_CH2, DMA_INT_FTF);
    /* enable DMA channel2 */
    dma_channel_enable(DMA_CH2);
    /* USART DMA enable for transmission */
    usart_dma_transmit_config(USART0, USART_DENT_ENABLE);
    /* enable DMA channel1 transfer complete interrupt */
    dma_interrupt_enable(DMA_CH1, DMA_INT_FTF);
    /* enable DMA channel1 */
    dma_channel_enable(DMA_CH1);

    /* waiting for the transfer to complete*/
    while(RESET == g_transfer_complete);

    g_transfer_complete = RESET;

    /* waiting for the transfer to complete*/
    while(RESET == g_transfer_complete);

    printf("\n\r%s\n\r", rxbuffer);
    while(1);
}
/*!
    \brief      configure DMA interrupt
    \param[in]  none
    \param[out] none
    \retval     none
*/
void nvic_config(void)
{
    nvic_irq_enable(DMA_Channel1_2_IRQn, 0, 0);
}

/*!
    \brief      configure the USART0 GPIO ports
    \param[in]  none
    \param[out] none
    \retval     none
*/
void usart0_gpio_config(void)
{
    /* enable COM GPIO clock */
    rcu_periph_clock_enable(RCU_GPIOA);

    /* connect port to USARTx_Tx */
    gpio_af_set(GPIOA, GPIO_AF_1, GPIO_PIN_9);

    /* connect port to USARTx_Rx */
    gpio_af_set(GPIOA, GPIO_AF_1, GPIO_PIN_10);

    /* configure USART Tx as alternate function push-pull */
    gpio_mode_set(GPIOA, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_9);
    gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_10MHZ, GPIO_PIN_9);

    /* configure USART Rx as alternate function push-pull */
    gpio_mode_set(GPIOA, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_10);
    gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_10MHZ, GPIO_PIN_10);
}

/*!
    \brief      configure the USART0
    \param[in]  none
    \param[out] none
    \retval     none
*/
void usart0_config(void)
{
    /* enable USART clock */
    rcu_periph_clock_enable(RCU_USART0);

    /* USART configure */
    usart_deinit(USART0);
    usart_baudrate_set(USART0, 115200U);
    usart_receive_config(USART0, USART_RECEIVE_ENABLE);
    usart_transmit_config(USART0, USART_TRANSMIT_ENABLE);

    usart_enable(USART0);
}


/* retarget the C library printf function to the USART */
int fputc(int ch, FILE *f)
{
    usart_data_transmit(USART0, (uint8_t)ch);
    while(RESET == usart_flag_get(USART0, USART_FLAG_TBE));
    return ch;
}



_______________________________________________________________________



#include "gd32f3x0_it.h"

extern FlagStatus g_transfer_complete;

/*!
    \brief      this function handles NMI exception
    \param[in]  none
    \param[out] none
    \retval     none
*/
void NMI_Handler(void)
{
}

/*!
    \brief      this function handles HardFault exception
    \param[in]  none
    \param[out] none
    \retval     none
*/
void HardFault_Handler(void)
{
    /* if Hard Fault exception occurs, go to infinite loop */
    while(1);
}

/*!
    \brief      this function handles MemManage exception
    \param[in]  none
    \param[out] none
    \retval     none
*/
void MemManage_Handler(void)
{
    /* if Memory Manage exception occurs, go to infinite loop */
    while(1);
}

/*!
    \brief      this function handles BusFault exception
    \param[in]  none
    \param[out] none
    \retval     none
*/
void BusFault_Handler(void)
{
    /* if Bus Fault exception occurs, go to infinite loop */
    while(1);
}

/*!
    \brief      this function handles UsageFault exception
    \param[in]  none
    \param[out] none
    \retval     none
*/
void UsageFault_Handler(void)
{
    /* if Usage Fault exception occurs, go to infinite loop */
    while(1);
}

/*!
    \brief      this function handles SVC exception
    \param[in]  none
    \param[out] none
    \retval     none
*/
void SVC_Handler(void)
{
}

/*!
    \brief      this function handles DebugMon exception
    \param[in]  none
    \param[out] none
    \retval     none
*/
void DebugMon_Handler(void)
{
}

/*!
    \brief      this function handles PendSV exception
    \param[in]  none
    \param[out] none
    \retval     none
*/
void PendSV_Handler(void)
{
}

/*!
    \brief      this function handles SysTick exception
    \param[in]  none
    \param[out] none
    \retval     none
*/
void SysTick_Handler(void)
{
}

/*!
    \brief      this function handles DMA_Channel1_2_IRQHandler interrupt
    \param[in]  none
    \param[out] none
    \retval     none
*/
void DMA_Channel1_2_IRQHandler(void)
{
    if(RESET != dma_interrupt_flag_get(DMA_CH1, DMA_INT_FLAG_FTF)) {
        dma_interrupt_flag_clear(DMA_CH1, DMA_INT_FLAG_G);
        g_transfer_complete = SET;
    }
    if(RESET != dma_interrupt_flag_get(DMA_CH2, DMA_INT_FLAG_FTF)) {
        dma_interrupt_flag_clear(DMA_CH2, DMA_INT_FLAG_G);
        g_transfer_complete = SET;
    }
}




-}
