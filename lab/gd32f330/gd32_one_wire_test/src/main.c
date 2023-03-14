#include <stdint.h>

#include "delay_us.h"
#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"
#include "gpio.h"
#include "one_wire.h"

extern pin one_wire, rede;
volatile float temperature;
uint16_t tmp;

int main() {
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);

  delay_init();

  pin_init_out(rede);
  pin_reset(rede);

  pin_init_out_OD(one_wire);
  pin_set(one_wire);

  // while(1){
  //   pin_set(one_wire);
  //   delay_us(400);
  //   pin_reset(one_wire);
  //   delay_us(1000);
  // }

  while (1) {
    while (1) {
      if (one_wire_reset() == 1) {
        // pin_reset(one_wire);
        break;
      } else {
        one_wire_send_byte(0xCC);
        one_wire_send_byte(0x44);
      }
      delay_us(600000);
      if (one_wire_reset() == 1) {
        pin_reset(one_wire);
        break;
      } else {
        one_wire_send_byte(0xCC);
        one_wire_send_byte(0xBE);
        tmp = one_wire_read_byte();
        tmp |= (uint16_t)(one_wire_read_byte()) << 8;
        temperature = (float)tmp / 16;
        break;
      }
    }
    delay_us(400000);
  }
}
