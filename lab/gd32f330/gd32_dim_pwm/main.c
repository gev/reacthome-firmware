#include <stdint.h>

#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"

/*!
    \file    main.c
    \brief   TIMER2 dma demo

    \version 2017-06-06, V1.0.0, firmware for GD32F3x0
    \version 2019-06-01, V2.0.0, firmware for GD32F3x0
    \version 2020-09-30, V2.1.0, firmware for GD32F3x0
    \version 2022-01-06, V2.2.0, firmware for GD32F3x0
*/

/*
    Copyright (c) 2022, GigaDevice Semiconductor Inc.

    Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.
    2. Redistributions in binary form must reproduce the above copyright notice,
       this list of conditions and the following disclaimer in the documentation
       and/or other materials provided with the distribution.
    3. Neither the name of the copyright holder nor the names of its
contributors may be used to endorse or promote products derived from this
software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>

#define TIMER15_CH0CC ((uint32_t)0x40014434)
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
void gpio_config(void) {
  rcu_periph_clock_enable(RCU_GPIOB);
  rcu_periph_clock_enable(RCU_GPIOA);

  gpio_mode_set(GPIOA, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_6);
  gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_6);
  gpio_af_set(GPIOA, GPIO_AF_1, GPIO_PIN_6);

  gpio_mode_set(GPIOA, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_7);
  gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_7);
  gpio_af_set(GPIOA, GPIO_AF_1, GPIO_PIN_7);

  gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_0);
  gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_0);
  gpio_af_set(GPIOB, GPIO_AF_1, GPIO_PIN_0);

  gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_1);
  gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_1);
  gpio_af_set(GPIOB, GPIO_AF_1, GPIO_PIN_1);
}

/*!
    \brief      configure the DMA peripheral
    \param[in]  none
    \param[out] none
    \retval     none
  */

/*!
    \brief      configure the TIMER peripheral
    \param[in]  none
    \param[out] none
    \retval     none
  */
void timer_config(void) {
  timer_oc_parameter_struct timer_ocintpara;
  timer_parameter_struct timer_initpara;

  rcu_periph_clock_enable(RCU_TIMER2);

  timer_deinit(TIMER2);

  timer_initpara.prescaler = 83;
  timer_initpara.alignedmode = TIMER_COUNTER_EDGE;
  timer_initpara.counterdirection = TIMER_COUNTER_UP;
  timer_initpara.period = 999;
  timer_initpara.clockdivision = TIMER_CKDIV_DIV1;
  timer_initpara.repetitioncounter = 0;
  timer_init(TIMER2, &timer_initpara);

  /* CH0 configuration in PWM0 mode */
  timer_ocintpara.outputstate = TIMER_CCX_ENABLE;
  timer_ocintpara.outputnstate = TIMER_CCXN_DISABLE;
  timer_ocintpara.ocpolarity = TIMER_OC_POLARITY_HIGH;
  timer_ocintpara.ocnpolarity = TIMER_OCN_POLARITY_HIGH;
  timer_ocintpara.ocidlestate = TIMER_OC_IDLE_STATE_HIGH;
  timer_ocintpara.ocnidlestate = TIMER_OCN_IDLE_STATE_LOW;
  timer_channel_output_config(TIMER2, TIMER_CH_0, &timer_ocintpara);

  timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_0, 200);
  timer_channel_output_mode_config(TIMER2, TIMER_CH_0, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(TIMER2, TIMER_CH_0,
                                     TIMER_OC_SHADOW_DISABLE);

  /* CH1 configuration in PWM0 mode */
  timer_ocintpara.outputstate = TIMER_CCX_ENABLE;
  timer_ocintpara.outputnstate = TIMER_CCXN_ENABLE;
  timer_ocintpara.ocpolarity = TIMER_OC_POLARITY_HIGH;
  timer_ocintpara.ocnpolarity = TIMER_OCN_POLARITY_HIGH;
  timer_ocintpara.ocidlestate = TIMER_OC_IDLE_STATE_HIGH;
  timer_ocintpara.ocnidlestate = TIMER_OCN_IDLE_STATE_LOW;
  timer_channel_output_config(TIMER2, TIMER_CH_1, &timer_ocintpara);

  timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_1, 400);
  timer_channel_output_mode_config(TIMER2, TIMER_CH_1, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(TIMER2, TIMER_CH_1,
                                     TIMER_OC_SHADOW_DISABLE);

  /* CH2 configuration in PWM0 mode */
  timer_ocintpara.outputstate = TIMER_CCX_ENABLE;
  timer_ocintpara.outputnstate = TIMER_CCXN_ENABLE;
  timer_ocintpara.ocpolarity = TIMER_OC_POLARITY_HIGH;
  timer_ocintpara.ocnpolarity = TIMER_OCN_POLARITY_HIGH;
  timer_ocintpara.ocidlestate = TIMER_OC_IDLE_STATE_HIGH;
  timer_ocintpara.ocnidlestate = TIMER_OCN_IDLE_STATE_LOW;
  timer_channel_output_config(TIMER2, TIMER_CH_2, &timer_ocintpara);

  timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_2, 600);
  timer_channel_output_mode_config(TIMER2, TIMER_CH_2, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(TIMER2, TIMER_CH_2,
                                     TIMER_OC_SHADOW_DISABLE);

  /* CH3 configuration in PWM0 mode */
  timer_ocintpara.outputstate = TIMER_CCX_ENABLE;
  timer_ocintpara.outputnstate = TIMER_CCXN_ENABLE;
  timer_ocintpara.ocpolarity = TIMER_OC_POLARITY_HIGH;
  timer_ocintpara.ocnpolarity = TIMER_OCN_POLARITY_HIGH;
  timer_ocintpara.ocidlestate = TIMER_OC_IDLE_STATE_HIGH;
  timer_ocintpara.ocnidlestate = TIMER_OCN_IDLE_STATE_LOW;
  timer_channel_output_config(TIMER2, TIMER_CH_3, &timer_ocintpara);

  timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_3, 800);
  timer_channel_output_mode_config(TIMER2, TIMER_CH_3, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(TIMER2, TIMER_CH_3,
                                     TIMER_OC_SHADOW_DISABLE);

  /* TIMER2 primary output enable */
  timer_primary_output_config(TIMER2, ENABLE);

  /* TIMER2 counter enable */
  timer_enable(TIMER2);
}

/*!
    \brief      main function
    \param[in]  none
    \param[out] none
    \retval     none
*/
int main(void) {
  gpio_config();
  timer_config();

  while (1) {
    for (uint32_t x = 0; x < 4000000; x++)
      ;
    timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_0, 200);
    for (uint32_t x = 0; x < 4000000; x++)
      ;
    timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_0, 400);
    for (uint32_t x = 0; x < 4000000; x++)
      ;
    timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_0, 600);
    for (uint32_t x = 0; x < 4000000; x++)
      ;
    timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_0, 800);
  }
}
