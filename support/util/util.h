#ifndef _UTIL_H_
#define _UTIL_H_

#include "stdint.h"
#include "core_cm4.h"
#include "system_gd32f3x0.h"

void delay_init(void);
inline void delay_us(uint32_t time);


#endif