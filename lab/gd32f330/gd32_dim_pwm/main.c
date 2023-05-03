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


void pin_config_af(uint32_t port, uint32_t pin, uint32_t af){
  gpio_mode_set(port, GPIO_MODE_AF, GPIO_PUPD_NONE, pin);
  gpio_output_options_set(port, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, pin);
  gpio_af_set(port, af, pin);
}



void timer_conf(uint32_t timer, uint32_t rcu) {
  timer_oc_parameter_struct timer_ocintpara;
  timer_parameter_struct timer_initpara;

  rcu_periph_clock_enable(rcu);

  timer_deinit(timer);

  timer_initpara.prescaler = 83;
  timer_initpara.alignedmode = TIMER_COUNTER_EDGE;
  timer_initpara.counterdirection = TIMER_COUNTER_UP;
  timer_initpara.period = 999;
  timer_initpara.clockdivision = TIMER_CKDIV_DIV1;
  timer_initpara.repetitioncounter = 0;
  timer_init(timer, &timer_initpara);

  /* CH0 configuration in PWM0 mode */
  timer_ocintpara.outputstate = TIMER_CCX_ENABLE;
  timer_ocintpara.outputnstate = TIMER_CCXN_DISABLE;
  timer_ocintpara.ocpolarity = TIMER_OC_POLARITY_HIGH;
  timer_ocintpara.ocnpolarity = TIMER_OCN_POLARITY_HIGH;
  timer_ocintpara.ocidlestate = TIMER_OC_IDLE_STATE_HIGH;
  timer_ocintpara.ocnidlestate = TIMER_OCN_IDLE_STATE_LOW;
  timer_channel_output_config(timer, TIMER_CH_0, &timer_ocintpara);

  timer_channel_output_pulse_value_config(timer, TIMER_CH_0, 0);
  timer_channel_output_mode_config(timer, TIMER_CH_0, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(timer, TIMER_CH_0,
                                     TIMER_OC_SHADOW_DISABLE);

  /* CH1 configuration in PWM0 mode */
  timer_ocintpara.outputstate = TIMER_CCX_ENABLE;
  timer_ocintpara.outputnstate = TIMER_CCXN_ENABLE;
  timer_ocintpara.ocpolarity = TIMER_OC_POLARITY_HIGH;
  timer_ocintpara.ocnpolarity = TIMER_OCN_POLARITY_HIGH;
  timer_ocintpara.ocidlestate = TIMER_OC_IDLE_STATE_HIGH;
  timer_ocintpara.ocnidlestate = TIMER_OCN_IDLE_STATE_LOW;
  timer_channel_output_config(timer, TIMER_CH_1, &timer_ocintpara);

  timer_channel_output_pulse_value_config(timer, TIMER_CH_1, 0);
  timer_channel_output_mode_config(timer, TIMER_CH_1, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(timer, TIMER_CH_1,
                                     TIMER_OC_SHADOW_DISABLE);

  /* CH2 configuration in PWM0 mode */
  timer_ocintpara.outputstate = TIMER_CCX_ENABLE;
  timer_ocintpara.outputnstate = TIMER_CCXN_ENABLE;
  timer_ocintpara.ocpolarity = TIMER_OC_POLARITY_HIGH;
  timer_ocintpara.ocnpolarity = TIMER_OCN_POLARITY_HIGH;
  timer_ocintpara.ocidlestate = TIMER_OC_IDLE_STATE_HIGH;
  timer_ocintpara.ocnidlestate = TIMER_OCN_IDLE_STATE_LOW;
  timer_channel_output_config(timer, TIMER_CH_2, &timer_ocintpara);

  timer_channel_output_pulse_value_config(timer, TIMER_CH_2, 0);
  timer_channel_output_mode_config(timer, TIMER_CH_2, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(timer, TIMER_CH_2,
                                     TIMER_OC_SHADOW_DISABLE);

  /* CH3 configuration in PWM0 mode */
  timer_ocintpara.outputstate = TIMER_CCX_ENABLE;
  timer_ocintpara.outputnstate = TIMER_CCXN_ENABLE;
  timer_ocintpara.ocpolarity = TIMER_OC_POLARITY_HIGH;
  timer_ocintpara.ocnpolarity = TIMER_OCN_POLARITY_HIGH;
  timer_ocintpara.ocidlestate = TIMER_OC_IDLE_STATE_HIGH;
  timer_ocintpara.ocnidlestate = TIMER_OCN_IDLE_STATE_LOW;
  timer_channel_output_config(timer, TIMER_CH_3, &timer_ocintpara);

  timer_channel_output_pulse_value_config(timer, TIMER_CH_3, 0);
  timer_channel_output_mode_config(timer, TIMER_CH_3, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(timer, TIMER_CH_3,
                                     TIMER_OC_SHADOW_DISABLE);

  /* timer primary output enable */
  timer_primary_output_config(timer, ENABLE);

  /* timer counter enable */
  timer_enable(timer);
}



/*!
    \brief      main function
    \param[in]  none
    \param[out] none
    \retval     none
*/
int main(void) {
  rcu_periph_clock_enable(RCU_GPIOB);
  rcu_periph_clock_enable(RCU_GPIOA);
  timer_conf(TIMER0, RCU_TIMER0);
  timer_conf(TIMER1, RCU_TIMER1);
  timer_conf(TIMER2, RCU_TIMER2);

  pin_config_af(GPIOA, GPIO_PIN_0,  GPIO_AF_2);
  pin_config_af(GPIOA, GPIO_PIN_1,  GPIO_AF_2);
  pin_config_af(GPIOA, GPIO_PIN_2,  GPIO_AF_2);
  pin_config_af(GPIOA, GPIO_PIN_3,  GPIO_AF_2);
  pin_config_af(GPIOA, GPIO_PIN_6,  GPIO_AF_1);
  pin_config_af(GPIOA, GPIO_PIN_7,  GPIO_AF_1);

  pin_config_af(GPIOA, GPIO_PIN_11, GPIO_AF_2);
  pin_config_af(GPIOB, GPIO_PIN_1,  GPIO_AF_1);
  pin_config_af(GPIOB, GPIO_PIN_0,  GPIO_AF_1);
  pin_config_af(GPIOA, GPIO_PIN_8,  GPIO_AF_2);
  pin_config_af(GPIOA, GPIO_PIN_9,  GPIO_AF_2);
  pin_config_af(GPIOA, GPIO_PIN_10, GPIO_AF_2);

  timer_channel_output_pulse_value_config(TIMER0, TIMER_CH_0, 500);
  timer_channel_output_pulse_value_config(TIMER0, TIMER_CH_1, 500);
  timer_channel_output_pulse_value_config(TIMER0, TIMER_CH_2, 500);
  timer_channel_output_pulse_value_config(TIMER0, TIMER_CH_3, 500);

  timer_channel_output_pulse_value_config(TIMER1, TIMER_CH_0, 500);
  timer_channel_output_pulse_value_config(TIMER1, TIMER_CH_1, 500);
  timer_channel_output_pulse_value_config(TIMER1, TIMER_CH_2, 500);
  timer_channel_output_pulse_value_config(TIMER1, TIMER_CH_3, 500);

  timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_0, 500);
  timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_1, 500);
  timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_2, 500);
  timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_3, 500);

  while (1) {

  }
}
