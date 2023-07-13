#ifndef _ONEWIRE_H_
#define _ONEWIRE_H_
#include "gd32f3x0.h"

void init_onewire(void);
void ow_reset(void);
void ow_write_buf(uint8_t *data, uint8_t size);

#endif