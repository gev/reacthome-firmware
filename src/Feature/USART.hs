{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}
{-# LANGUAGE FlexibleContexts #-}

module Feature.USART  where

import           Device.GD32F3x0.SystemClock
import           Feature
import qualified Interface                   as I
import qualified Interface.Timer             as I
import qualified Interface.USART             as I
import           Ivory.Language
import           Ivory.Stdlib


data USART = forall a. (I.USART a) => USART Int a

instance I.Interface USART where

  dependencies (USART n usart) = defMemArea (t0'' n)
                               : defMemArea (buff'' n)
                               : defMemArea (index'' n)
                               : I.dependencies usart
                               

  initialize (USART n usart) = I.initialize usart <> [
    proc ("usart_" <> show n <> "_init") $ body $ do
      I.setBaudrate   usart 1_000_000
      I.setWordLength usart I.WL_8b
      I.setParity     usart I.None
      I.enable        usart
    ]


t0'' :: Int -> MemArea (Stored Uint32)
t0'' n = area ("t0_" <> show n) $ Just (ival 0)

t0' :: Int -> Ref Global (Stored Uint32)
t0' = addrOf . t0''


index'' :: Int -> MemArea (Stored Uint16)
index'' n = area ("index_" <> show n) $ Just (ival 0)

index' :: Int -> Ref Global (Stored Uint16)
index' = addrOf . index''


buff'' :: Int -> MemArea (Array 512 (Stored Uint16))
buff'' n = area ("buff_" <> show n) $ Just (iarray [ival 0])

buff' :: Int -> Ref Global (Array 512 (Stored Uint16))
buff' = addrOf . buff''


instance Task USART where
  tasks (USART n usart) = [
    Step Nothing $ proc ("usart_" <> show n <> "_step") $ body $ do
      forever $ do
                hasReceived <- I.hasReceived usart
                ifte_ hasReceived 
                      ( do
                        index <- deref $ index' n
                        let ix = toIx index
                        store (buff' n ! ix) =<< I.receive usart
                        store (index' n) (index + 1)
                        store (t0' n) =<< I.readCounter systemClock
                      )
                      breakOut
      -- forever $ do 
      --           t1 <- I.readCounter systemClock
      --           t0 <- deref $ t0' n
      --           when ( t1 - t0 >? 400 ) breakOut
      index <- deref $ index' n
      let ix = toIx index
      for ix $ \i -> do
        forever $ do
          canTransmit <- I.canTransmit usart
          when canTransmit breakOut
        I.transmit usart =<< deref (buff' n ! i)
      store (index' n) 0

    ]


{-
#include "gd32f3x0.h"                   
#include "gd32f3x0_gpio.h"              
#include "gd32f3x0_usart.h"               
#include <stdint.h>
#include <string.h>

uint8_t transmitter_buffer[12];
uint8_t receiver_buffer[12];
uint8_t transfersize = 12;
uint8_t receivesize = 12;
uint8_t txcount = 0;
uint16_t rxcount = 0;

void uart_init(void);

int main(void)
{
    /* USART interrupt configuration */
    nvic_irq_enable(USART1_IRQn, 0, 0);

    /* initilize the com */
    uart_init();
    usart_interrupt_enable(USART1, USART_INT_RBNE);

    while(1);
}

/*!
    \brief      configure the USART1 GPIO ports
    \param[in]  none
    \param[out] none
    \retval     none
*/
void uart_init(void){
	rcu_periph_clock_enable(RCU_GPIOA);
	rcu_periph_clock_enable(RCU_USART1);

	/*RE_DE*/
	gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_4);		 
	gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_4);
	gpio_bit_reset(GPIOA, GPIO_PIN_4);

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



    /* USART configure */
    usart_deinit(USART1);
    usart_word_length_set(USART1, USART_WL_8BIT);
    usart_stop_bit_set(USART1, USART_STB_1BIT);
    usart_parity_config(USART1, USART_PM_NONE);
    usart_baudrate_set(USART1, 1000000U);
    usart_receive_config(USART1, USART_RECEIVE_ENABLE);
    usart_transmit_config(USART1, USART_TRANSMIT_ENABLE);

    usart_enable(USART1);

}

void USART1_IRQHandler(void){
    if(usart_interrupt_flag_get(USART1, USART_INT_FLAG_RBNE)) {
        /* receive data */
        receiver_buffer[rxcount++] = usart_data_receive(USART1);
        if(rxcount == receivesize) {
            rxcount = 0;
            memcpy(transmitter_buffer, receiver_buffer, receivesize);
            usart_interrupt_disable(USART1, USART_INT_RBNE);
            usart_interrupt_enable(USART1, USART_INT_TBE);
        }
    }

    else if (usart_interrupt_flag_get(USART1, USART_INT_FLAG_TBE)) {
        /* transmit data */
        usart_data_transmit(USART1, transmitter_buffer[txcount++]);
        if(txcount == transfersize) {
            txcount = 0;
            usart_interrupt_disable(USART1, USART_INT_TBE);
            usart_interrupt_enable(USART1, USART_INT_RBNE);
        }
    }
}

-}