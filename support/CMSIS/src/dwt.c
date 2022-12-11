#include "dwt.h"

void dwt_delay_init(void){
  CoreDebug->DEMCR |= CoreDebug_DEMCR_TRCENA_Msk;
  DWT->CTRL |= DWT_CTRL_CYCCNTENA_Msk;
}

void dwt_delay(uint32_t ticks) {
  uint32_t start_tick = DWT->CYCCNT;
  while (DWT->CYCCNT - start_tick < ticks);
}


//-----------------------

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
