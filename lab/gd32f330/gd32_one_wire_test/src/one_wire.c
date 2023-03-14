#include "one_wire.h"
#include "gpio.h"
#include "delay_us.h"


#define TIME_RESET_US 480
#define TIME_PRESENCE_FIRST_US 70
#define TIME_PRESENCE_SECOND_US 410
#define TIME_SEND_BIT_1_US 6
#define TIME_SEND_BIT_1_PAUSE_US 64
#define TIME_SEND_BIT_0_US 60
#define TIME_SEND_BIT_0_PAUSE_US 10
#define TIME_READ_BIT_US 9
#define TIME_READ_BIT_PAUSE_US 55

extern pin one_wire, rede;

void one_wire_init() {
  pin_set(one_wire);
}

uint8_t one_wire_reset() {
  pin_reset(one_wire);
  delay_us(TIME_RESET_US);
  pin_set(one_wire);
  delay_us(TIME_PRESENCE_FIRST_US);
  if (pin_get(one_wire) == 1) return 1;
  delay_us(TIME_PRESENCE_SECOND_US);
  if (pin_get(one_wire) == 0) return 1;
  return 0;
}

void one_wire_send_byte(uint8_t data) {
  for (uint8_t bit = 0; bit < 8; bit++) {
    pin_reset(one_wire);
    if (0 < (data & 1)) {
      delay_us(TIME_SEND_BIT_1_US);
      pin_set(one_wire);
      delay_us(TIME_SEND_BIT_1_PAUSE_US);
    } else {
      delay_us(TIME_SEND_BIT_0_US);
      pin_set(one_wire);
      delay_us(TIME_SEND_BIT_0_PAUSE_US);
    }
    data >>= 1;
  }
}

uint8_t one_wire_read_byte(void) {
  uint8_t result = 0;
  for (uint8_t bit = 0; bit < 8; bit++) {
    pin_reset(one_wire);
    delay_us(TIME_SEND_BIT_1_US);
    pin_set(one_wire);
    delay_us(TIME_READ_BIT_US);
    if (pin_get(one_wire) == 1)
      result |= (1 << bit);
    delay_us(TIME_READ_BIT_PAUSE_US);
  }
  return result;
}