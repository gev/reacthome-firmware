#include "dwt.h"

void dwt_delay_init(void){
  CoreDebug->DEMCR |= CoreDebug_DEMCR_TRCENA_Msk;
  DWT->CTRL |= DWT_CTRL_CYCCNTENA_Msk;
}

void dwt_delay(uint32_t ticks) {
  uint32_t start_tick = DWT->CYCCNT;
  while (DWT->CYCCNT - start_tick < ticks);
}

inline void delay_us(uint32_t time){
  uint32_t ticks = 84 * time;
  uint32_t t, dt;
  //SysTick->VAL = 0;
  // SysTick->LOAD = 0xFFFFFF;
  // SysTick->CTRL |= SysTick_CTRL_ENABLE_Msk;
  uint32_t start_tick = SysTick->VAL;
  do{
    t = SysTick->VAL;
    dt = t < start_tick ? start_tick - t : SysTick->LOAD - t + start_tick;
  }
  while (dt < ticks);
  // while(SysTick->VAL < ticks );
}


int main() {
  rcu_periph_clock_enable(RCU_GPIOA);                                  
  gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_4);  // re/de
  gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_4);
  gpio_bit_reset(GPIOA, GPIO_PIN_4);
  // SysTick_Config(0xFFFFFF);

  SysTick->CTRL  = SysTick_CTRL_CLKSOURCE_Msk;    
  SysTick->LOAD = 83999;
  SysTick->CTRL |= SysTick_CTRL_ENABLE_Msk;
  while (1) {
    gpio_bit_reset(GPIOA, GPIO_PIN_4);
    delay_us(999);
    gpio_bit_set(GPIOA, GPIO_PIN_4);
    delay_us(999);
  }
}
