#ifndef _DELAY_H_
#define _DELAY_H_
#include <stdint.h>
#include "gd32f3x0.h"

void delay_init(void);
void delay_us(uint32_t time);
void delay_ms(uint32_t time);

#endif