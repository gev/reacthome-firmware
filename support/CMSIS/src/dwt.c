#include "dwt.h"

void dwt_delay_init(void){
  CoreDebug->DEMCR |= CoreDebug_DEMCR_TRCENA_Msk;
  DWT->CTRL |= DWT_CTRL_CYCCNTENA_Msk;
}

inline void dwt_delay(uint32_t ticks) {
  uint32_t start_tick = dwt_cycle_counter;
  while (dwt_cycle_counter - start_tick < ticks);
}
