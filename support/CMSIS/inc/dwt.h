#ifndef UTIL_H
#define UTIL_H

#include "stdint.h"
#include "core_cm4.h"

#define dwt_cycle_counter   DWT->CYCCNT

void dwt_delay_init(void);
inline void dwt_delay(uint32_t ticks);

#endif
