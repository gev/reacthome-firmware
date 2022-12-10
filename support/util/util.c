#include "util.h"

void delay_init(void){
  CoreDebug->DEMCR |= CoreDebug_DEMCR_TRCENA_Msk;
  DWT->CTRL |= DWT_CTRL_CYCCNTENA_Msk;
}

inline void delay_us(uint32_t time){
  uint32_t ticks = SystemCoreClock / 1000000 * time;
  uint32_t start_tick = DWT->CYCCNT;
  while (DWT->CYCCNT - start_tick < ticks);
}