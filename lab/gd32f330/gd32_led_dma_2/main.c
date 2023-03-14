#include <stdint.h>

#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"
#include "sk6812.h"





/*!
    \file    main.c
    \brief   TIMER15 dma demo

    \version 2017-06-06, V1.0.0, firmware for GD32F3x0
    \version 2019-06-01, V2.0.0, firmware for GD32F3x0
    \version 2020-09-30, V2.1.0, firmware for GD32F3x0
    \version 2022-01-06, V2.2.0, firmware for GD32F3x0
*/

/*
    Copyright (c) 2022, GigaDevice Semiconductor Inc.

    Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this
       list of conditions and the following disclaimer.
    2. Redistributions in binary form must reproduce the above copyright notice,
       this list of conditions and the following disclaimer in the documentation
       and/or other materials provided with the distribution.
    3. Neither the name of the copyright holder nor the names of its contributors
       may be used to endorse or promote products derived from this software without
       specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
OF SUCH DAMAGE.
*/


#include <stdio.h>

#define TIMER15_CH0CC  ((uint32_t)0x40014434)
uint16_t buffer[3] = {249, 499, 749};

void gpio_config(void);
void timer_config(void);
void dma_config(void);

/*!
    \brief      configure the GPIO ports
    \param[in]  none
    \param[out] none
    \retval     none
  */
void gpio_config(void)
{
    rcu_periph_clock_enable(RCU_GPIOB);

    /*configure PA8(TIMER15 CH0) as alternate function*/
    gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_8);
    gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_8);

    gpio_af_set(GPIOB, GPIO_AF_2, GPIO_PIN_8);
}


/*!
    \brief      configure the DMA peripheral
    \param[in]  none
    \param[out] none
    \retval     none
  */
void dma_config(void)
{
    dma_parameter_struct dma_init_struct;

    /* enable DMA clock */
    rcu_periph_clock_enable(RCU_DMA);

    /* initialize DMA channel4 */
    dma_deinit(DMA_CH0);

    /* DMA channel4 initialize */
    dma_deinit(DMA_CH0);
    dma_init_struct.direction    = DMA_MEMORY_TO_PERIPHERAL;
    dma_init_struct.memory_addr  = (uint32_t)buffer;
    dma_init_struct.memory_inc   = DMA_MEMORY_INCREASE_ENABLE;
    dma_init_struct.memory_width = DMA_MEMORY_WIDTH_16BIT;
    dma_init_struct.number       = 3;
    dma_init_struct.periph_addr  = (uint32_t)TIMER15_CH0CC;
    dma_init_struct.periph_inc   = DMA_PERIPH_INCREASE_DISABLE;
    dma_init_struct.periph_width = DMA_PERIPHERAL_WIDTH_16BIT;
    dma_init_struct.priority     = DMA_PRIORITY_HIGH;
    dma_init(DMA_CH0, &dma_init_struct);

    /* configure DMA mode */
    dma_circulation_enable(DMA_CH0);
    dma_memory_to_memory_disable(DMA_CH0);

    /* enable DMA channel4 */
    dma_channel_enable(DMA_CH0);
}

/*!
    \brief      configure the TIMER peripheral
    \param[in]  none
    \param[out] none
    \retval     none
  */
void timer_config(void)
{
    /* TIMER15 DMA Transfer example -------------------------------------------------
    TIMER15CLK = 84MHz(GD32F330) or 108MHz(GD32F350), Prescaler = 84(GD32F330) or 108(GD32F350)
    TIMER15 counter clock 1MHz.

    the objective is to configure TIMER15 channel 1 to generate PWM
    signal with a frequency equal to 1KHz and a variable duty cycle(25%,50%,75%) that is
    changed by the DMA after a specific number of update DMA request.

    the number of this repetitive requests is defined by the TIMER15 repetition counter,
    each 2 update requests, the TIMER15 Channel 0 duty cycle changes to the next new
    value defined by the buffer .
    -----------------------------------------------------------------------------*/
    timer_oc_parameter_struct timer_ocintpara;
    timer_parameter_struct timer_initpara;

    rcu_periph_clock_enable(RCU_TIMER15);

    timer_deinit(TIMER15);

    /* TIMER15 configuration */
#ifdef GD32F330
    timer_initpara.prescaler         = 83;
#endif /* GD32F330 */
#ifdef GD32F350
    timer_initpara.prescaler         = 107;
#endif /* GD32F350 */
    timer_initpara.alignedmode       = TIMER_COUNTER_EDGE;
    timer_initpara.counterdirection  = TIMER_COUNTER_UP;
    timer_initpara.period            = 999;
    timer_initpara.clockdivision     = TIMER_CKDIV_DIV1;
    timer_initpara.repetitioncounter = 1;
    timer_init(TIMER15, &timer_initpara);

    /* CH0 configuration in PWM0 mode */
    timer_ocintpara.outputstate  = TIMER_CCX_ENABLE;
    timer_ocintpara.outputnstate = TIMER_CCXN_ENABLE;
    timer_ocintpara.ocpolarity   = TIMER_OC_POLARITY_HIGH;
    timer_ocintpara.ocnpolarity  = TIMER_OCN_POLARITY_HIGH;
    timer_ocintpara.ocidlestate  = TIMER_OC_IDLE_STATE_HIGH;
    timer_ocintpara.ocnidlestate = TIMER_OCN_IDLE_STATE_LOW;
    timer_channel_output_config(TIMER15, TIMER_CH_0, &timer_ocintpara);

    timer_channel_output_pulse_value_config(TIMER15, TIMER_CH_0, buffer[0]);
    timer_channel_output_mode_config(TIMER15, TIMER_CH_0, TIMER_OC_MODE_PWM0);
    timer_channel_output_shadow_config(TIMER15, TIMER_CH_0, TIMER_OC_SHADOW_DISABLE);

    /* TIMER15 primary output enable */
    timer_primary_output_config(TIMER15, ENABLE);

    /* TIMER15 update DMA request enable */
    timer_dma_enable(TIMER15, TIMER_DMA_UPD);

    /* auto-reload preload enable */
    timer_auto_reload_shadow_enable(TIMER15);

    /* TIMER15 counter enable */
    timer_enable(TIMER15);
}

/*!
    \brief      main function
    \param[in]  none
    \param[out] none
    \retval     none
*/
int main(void)
{
    gpio_config();
    dma_config();
    timer_config();

    while(1);
}

















// #define num 1
// uint16_t RGB_buffer[] = {28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
//                          28, 28, 28, 28, 61, 61, 61, 61, 61, 61, 61, 61};

// void timers_dma_pin_init(void);
// void setRGB(uint8_t red, uint8_t green, uint8_t blue);

// int main() {
//   SystemInit();
//   rcu_periph_clock_enable(RCU_GPIOA);
//   rcu_periph_clock_enable(RCU_GPIOB);

//   gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_4);  // rede
//   gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_4);
//   gpio_bit_reset(GPIOA, GPIO_PIN_4);

//   gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_15);
//   gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_15);

//   timers_dma_pin_init();

//   while (1) {
//     /* initialize DMA channel */
//     // dma_deinit(DMA_CH0);
//     // /* DMAx_CHx initialize */

//     // dma_parameter_struct dma_init_struct = {
//     //     .direction = DMA_MEMORY_TO_PERIPHERAL,
//     //     .memory_addr = (uint32_t)RGB_buffer,
//     //     .memory_inc = DMA_MEMORY_INCREASE_ENABLE,
//     //     .memory_width = DMA_MEMORY_WIDTH_16BIT,
//     //     .number = (uint32_t)sizeof(RGB_buffer) / 2,
//     //     .periph_addr = (uint32_t)&TIMER_CH0CV(TIMER15),
//     //     .periph_inc = DMA_PERIPH_INCREASE_DISABLE,
//     //     .periph_width = DMA_PERIPHERAL_WIDTH_16BIT,
//     //     .priority = DMA_PRIORITY_HIGH,
//     // };
//     // dma_init(DMA_CH0, &dma_init_struct);

//     // /* update addresses of peripheral and memory */
    // dma_periph_address_config(DMA_CH0, (uint32_t)&TIMER_CH0CV(TIMER15));

//     /* DMAx_CHx mode configuration */
//     // dma_circulation_disable(DMA_CH0);
//     // dma_memory_to_memory_disable(DMA_CH0);

//     // dma_channel_enable(DMA_CH0);

//     for (uint32_t x = 0; x < 15000000; x++)
//       ;
//   }
// }

// void timers_dma_pin_init(void) {
//   rcu_periph_clock_enable(RCU_GPIOA);
//   rcu_periph_clock_enable(RCU_GPIOB);

//   gpio_af_set(GPIOB, GPIO_AF_2, GPIO_PIN_8);
//   gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_8);  // LED
//   gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_8);

//   //   //////////////////////////////////////////////////////////////////////////////////////

//   rcu_periph_clock_enable(RCU_DMA);
//   /* initialize DMA channel */
//   dma_deinit(DMA_CH0);
//   /* DMAx_CHx initialize */

//   dma_parameter_struct dma_init_struct = {
//       .direction = DMA_MEMORY_TO_PERIPHERAL,
//       .memory_addr = (uint32_t)RGB_buffer,
//       .memory_inc = DMA_MEMORY_INCREASE_ENABLE,
//       .memory_width = DMA_MEMORY_WIDTH_16BIT,
//       .number = (uint32_t)sizeof(RGB_buffer) / 2,
      // .periph_addr = (uint32_t)&TIMER_CH0CV(TIMER15),
//       .periph_inc = DMA_PERIPH_INCREASE_DISABLE,
//       .periph_width = DMA_PERIPHERAL_WIDTH_16BIT,
//       .priority = DMA_PRIORITY_HIGH,
//   };
//   dma_init(DMA_CH0, &dma_init_struct);

//   // /* update addresses of peripheral and memory */
//   // dma_periph_address_config(DMA_CH0, (uint32_t)&TIMER_CH0CV(TIMER15));

//   /* DMAx_CHx mode configuration */
//   dma_circulation_enable(DMA_CH0);
//   dma_memory_to_memory_disable(DMA_CH0);

//   dma_channel_enable(DMA_CH0);

//   //////////////////////////////////////////////////////////////////////////////////////

//   // timer_deinit(TIMER15);
//   rcu_periph_clock_enable(RCU_TIMER15);
//   timer_parameter_struct timer15_init = {
//       .prescaler = 0,
//       .alignedmode = TIMER_COUNTER_EDGE,
//       .counterdirection = TIMER_COUNTER_UP,
//       .period = 100,  // 1.2 us
//       .clockdivision = TIMER_CKDIV_DIV1,
//       .repetitioncounter = 1,
//   };
//   timer_init(TIMER15, &timer15_init);

//   timer_oc_parameter_struct timer15_init_out = {
//       .outputstate = TIMER_CCX_ENABLE,
//       .outputnstate = TIMER_CCXN_ENABLE,
//       .ocpolarity = TIMER_OC_POLARITY_HIGH,
//       .ocnpolarity = TIMER_OCN_POLARITY_HIGH,
//       .ocidlestate = TIMER_OC_IDLE_STATE_HIGH,
//       .ocnidlestate = TIMER_OCN_IDLE_STATE_LOW};
//   timer_channel_output_config(TIMER15, TIMER_CH_0, &timer15_init_out);

//   /* channel configuration in PWM mode0*/
//   timer_channel_output_pulse_value_config(TIMER15, TIMER_CH_0, 60);
//   timer_channel_output_mode_config(TIMER15, TIMER_CH_0, TIMER_OC_MODE_PWM0);
//   timer_channel_output_shadow_config(TIMER15, TIMER_CH_0,
//                                      TIMER_OC_SHADOW_DISABLE);

                                     
//   timer_primary_output_config(TIMER15, ENABLE);

//   /* send DMA requests at TIM_update event */
//   // timer_channel_dma_request_source_select(TIMER15,
//   //                                         TIMER_DMAREQUEST_UPDATEEVENT);
//   /* TIMERs update DMA request for capture compare on channel X */
//   timer_dma_enable(TIMER15, TIMER_DMA_UPD);

//   /* shadow register for auto-reload preload enable */
//   timer_auto_reload_shadow_enable(TIMER15);
//   /* auto-reload preload enable */
//   timer_enable(TIMER15);

//   // dma_flag_clear(DMA_CH0, DMA_INTC_FTFIFC);
//   // dma_channel_disable(DMA_CH0);
//   // dma_transfer_number_config(DMA_CH0, sizeof(RGB_buffer) / 2);
//   // dma_channel_enable(DMA_CH0);
// }
