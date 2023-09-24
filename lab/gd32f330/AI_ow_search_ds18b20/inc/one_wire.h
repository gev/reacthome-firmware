#ifndef _ONE_WIRE_
#define _ONE_WIRE_

#include <stdint.h>
#include "gd32f3x0.h"

#define MATCH_ROM               0x55
#define SEARCH_ROM_NORMAL       0xF0
#define SEARCH_ROM_CONDITIONAL  0xEC

void one_wire_init(void);
bool one_wire_reset(void);
uint8_t one_wire_read_byte(void);
uint8_t one_wire_read_bit(void);
void one_wire_write_byte(uint8_t);
void one_wire_write_bit(uint8_t);

#endif