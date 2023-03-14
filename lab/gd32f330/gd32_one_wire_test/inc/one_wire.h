#ifndef _ONE_WIRE_
#define _ONE_WIRE_

#include <stdint.h>

void one_wire_init(void);
uint8_t one_wire_reset(void);
uint8_t one_wire_read_byte(void);
void one_wire_send_byte(uint8_t);

#endif