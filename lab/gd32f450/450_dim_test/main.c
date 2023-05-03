#include "gd32f4xx.h"
#include "stdint.h"


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

  timer_initpara.prescaler = 199;
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



int main() {
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);
  rcu_periph_clock_enable(RCU_GPIOC);
  rcu_periph_clock_enable(RCU_GPIOD);
  rcu_periph_clock_enable(RCU_GPIOE);

  timer_conf(TIMER3, RCU_TIMER3);

  pin_config_af(GPIOD, GPIO_PIN_12, GPIO_AF_2);
  pin_config_af(GPIOD, GPIO_PIN_13, GPIO_AF_2);
  pin_config_af(GPIOD, GPIO_PIN_14, GPIO_AF_2);

  timer_channel_output_pulse_value_config(TIMER3, TIMER_CH_0, 600);
  timer_channel_output_pulse_value_config(TIMER3, TIMER_CH_1, 600);
  timer_channel_output_pulse_value_config(TIMER3, TIMER_CH_2, 600);

  while (1) {

  }
}
