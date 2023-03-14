#include <stdint.h>

// #include "core_cm4.h"
#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_usart.h"


void delay_us(uint32_t time){
  uint32_t start_tick = TIMER_CNT(TIMER1);
  while (TIMER_CNT(TIMER1) - start_tick < time);
}

void timer_config(void) {

  timer_parameter_struct timer_initpara;

  /* enable the peripherals clock */
  rcu_periph_clock_enable(RCU_TIMER1);

  /* deinit a TIMER */
  timer_deinit(TIMER1);
  /* initialize TIMER init parameter struct */
  timer_struct_para_init(&timer_initpara);
  /* TIMER1 configuration */
  timer_initpara.prescaler = 83;
  //   timer_initpara.alignedmode       = TIMER_COUNTER_EDGE;
  timer_initpara.counterdirection = TIMER_COUNTER_UP;
  timer_initpara.period = 0xFFFFFFFF;
  //   timer_initpara.clockdivision     = TIMER_CKDIV_DIV1;
  timer_init(TIMER1, &timer_initpara);
  timer_enable(TIMER1);
}



int main() {
  timer_config();
  rcu_periph_clock_enable(RCU_GPIOA);
  gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_4);  // re/de
  gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_MAX, GPIO_PIN_4);
  gpio_bit_reset(GPIOA, GPIO_PIN_4);
  SysTick_Config(83999);
  
  while (1) {
    gpio_bit_reset(GPIOA, GPIO_PIN_4);
    delay_us(100000);
    gpio_bit_set(GPIOA, GPIO_PIN_4);
    delay_us(100000);
  }
}
