#include <stdint.h>
#include "onewire.h"
#include "gd32f3x0.h"


int main(){
  init_onewire();
  // rcu_periph_clock_enable(RCU_GPIOA);
  // gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_8);
  // gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_MAX, GPIO_PIN_8);
  while(1){
    for (uint32_t i = 0; i < 10000000; i++)
		{}
    uint8_t d = 0x44;
    ow_reset();
    for (uint32_t i = 0; i < 50000; i++);
    ow_write_buf(&(d), 1);
  }
}

// void timer_config(void) {

//   timer_parameter_struct timer_initpara;

//   /* enable the peripherals clock */
//   rcu_periph_clock_enable(RCU_TIMER1);

//   /* deinit a TIMER */
//   timer_deinit(TIMER1);
//   /* initialize TIMER init parameter struct */
//   timer_struct_para_init(&timer_initpara);
//   /* TIMER1 configuration */
//   timer_initpara.prescaler = 83;
//   //   timer_initpara.alignedmode       = TIMER_COUNTER_EDGE;
//   timer_initpara.counterdirection = TIMER_COUNTER_UP;
//   timer_initpara.period = 0xFFFFFFFF;
//   //   timer_initpara.clockdivision     = TIMER_CKDIV_DIV1;
//   timer_init(TIMER1, &timer_initpara);
//   timer_enable(TIMER1);
// }