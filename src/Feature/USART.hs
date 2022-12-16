{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.USART  where

import           Device.GD32F3x0.SystemClock
import           Feature
import qualified Interface                     as I
import qualified Interface.Timer               as I
import qualified Interface.USART               as I
import           Ivory.Language
import           Ivory.Stdlib
import qualified Support.Device.GD32F3x0.USART as S


data USART = forall a. (I.USART a) => USART Int (I.OnReceive -> a)

instance I.Interface USART where

  dependencies (USART n usart) = defMemArea     (timestamp'' n)
                               : defMemArea     (buff'' n)
                               : defMemArea     (index'' n)
                               : I.dependencies (usart $ onReceive n)


  initialize (USART n usart) = I.initialize u <> [
    proc ("usart_" <> show n <> "_init") $ body $ do
      I.setBaudrate   u 1_000_000
      I.setWordLength u I.WL_8b
      I.setParity     u I.None
      I.enable        u
    ]
    where u = usart $ onReceive n


instance Task USART where
  tasks (USART n usart) = [
    Step Nothing $ proc ("usart_" <> show n <> "_step") $ body $ do
      timestamp <- deref $ timestamp' n
      t <- I.readCounter systemClock
      when (t - timestamp >? 400) $ do
        index <- deref $ index' n
        I.transmit u (buff' n) index
        store (index' n) 0
    ]
    where u = usart $ onReceive n


onReceive :: Int -> I.OnReceive
onReceive n b = do
    index <- deref $ index' n
    let ix = toIx index
    store (buff' n ! ix) b
    store (index' n) (index + 1)
    store (timestamp' n) =<< I.readCounter systemClock


timestamp'' :: Int -> MemArea (Stored Uint32)
timestamp'' n = area ("timestamp_" <> show n) $ Just (ival 0)

timestamp' :: Int -> Ref Global (Stored Uint32)
timestamp' = addrOf . timestamp''


index'' :: Int -> MemArea (Stored Uint16)
index'' n = area ("index_" <> show n) $ Just (ival 0)

index' :: Int -> Ref Global (Stored Uint16)
index' = addrOf . index''


buff'' :: Int -> MemArea (Array 512 (Stored Uint16))
buff'' n = area ("buff_" <> show n) $ Just (iarray [ival 0])

buff' :: Int -> Ref Global (Array 512 (Stored Uint16))
buff' = addrOf . buff''



{-
#include "gd32f3x0.h"
#include <stdio.h>


uint8_t rxbuffer[10];
uint8_t txbuffer[] = "\n\rUSART DMA receive and transmit example, please input 10 bytes:\n\r";
#define ARRAYNUM(arr_nanme)      (uint32_t)(sizeof(arr_nanme) / sizeof(*(arr_nanme)))

void usart1_gpio_config(void);
void usart1_config(void);

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
    usart1_gpio_config();
    usart1_config();

    dma_struct_para_init(&dma_init_struct);
    dma_init_struct.direction = DMA_MEMORY_TO_PERIPHERAL;
    dma_init_struct.memory_addr = (uint32_t)txbuffer;
    dma_init_struct.memory_inc = DMA_MEMORY_INCREASE_ENABLE;
    dma_init_struct.memory_width = DMA_MEMORY_WIDTH_8BIT;
    dma_init_struct.number = ARRAYNUM(txbuffer);
    dma_init_struct.periph_addr = (uint32_t) &USART_TDATA(USART1);
    dma_init_struct.periph_inc = DMA_PERIPH_INCREASE_DISABLE;
    dma_init_struct.periph_width = DMA_PERIPHERAL_WIDTH_8BIT;
    dma_init_struct.priority = DMA_PRIORITY_ULTRA_HIGH;
    while(1) {
    /* deinitialize DMA channel1 */
    dma_deinit(DMA_CH3);

    dma_init(DMA_CH3, &dma_init_struct);
    /* configure DMA mode */
    dma_circulation_disable(DMA_CH3);
    dma_memory_to_memory_disable(DMA_CH3);
    /* enable DMA channel1 */
    dma_channel_enable(DMA_CH3);
    

        /* USART DMA enable for transmission and reception */
        usart_dma_transmit_config(USART1, USART_DENT_ENABLE);
        /* wait DMA Channel transfer complete */
        while(RESET == dma_flag_get(DMA_CH3, DMA_FLAG_FTF));
    }
}

/*!
    \brief      configure the USART1 GPIO ports
    \param[in]  none
    \param[out] none
    \retval     none
*/
void usart1_gpio_config(void)
{
    /* enable COM GPIO clock */
    rcu_periph_clock_enable(RCU_GPIOA);

    /* connect port to USARTx_Tx */
    gpio_af_set(GPIOA, GPIO_AF_1, GPIO_PIN_2);

    /* connect port to USARTx_Rx */
    gpio_af_set(GPIOA, GPIO_AF_1, GPIO_PIN_3);

    /* configure USART Tx as alternate function push-pull */
    gpio_mode_set(GPIOA, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_2);
    gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_10MHZ, GPIO_PIN_2);

    /* configure USART Rx as alternate function push-pull */
    gpio_mode_set(GPIOA, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_3);
    gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_10MHZ, GPIO_PIN_3);
}

/*!
    \brief      configure the USART1
    \param[in]  none
    \param[out] none
    \retval     none
*/
void usart1_config(void)
{
    /* enable USART clock */
    rcu_periph_clock_enable(RCU_USART1);

    /* USART configure */
    usart_deinit(USART1);
    usart_baudrate_set(USART1, 1000000U);
    usart_receive_config(USART1, USART_RECEIVE_ENABLE);
    usart_transmit_config(USART1, USART_TRANSMIT_ENABLE);
    usart_enable(USART1);
}


-}
