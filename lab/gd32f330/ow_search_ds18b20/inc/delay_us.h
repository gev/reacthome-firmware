#ifndef _DELAY_US_H_
#define _DELAY_US_H_
#include <stdint.h>
#include "gd32f3x0.h"

void delay_init(void);
void delay_us(uint32_t time);

#endif