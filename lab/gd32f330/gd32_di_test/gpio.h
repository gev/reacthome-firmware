#ifndef _GPIO_H_
#define _GPIO_H_

#include "gd32f3x0.h"
#include "stdint.h"

typedef struct {
  uint32_t port;
  uint32_t num;
} pin;

void pin_init_input(pin x);
void pin_init_out(pin x);
void pin_init_out_OD(pin x);
void pin_toggle(pin x);
void pin_set(pin x);
void pin_reset(pin x);
bool pin_get(pin x);

#endif