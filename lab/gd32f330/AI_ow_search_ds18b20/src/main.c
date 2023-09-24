#include <stdint.h>

#include "delay_us.h"
#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"
#include "gpio.h"
#include "one_wire.h"
#include "ds18b20.h"

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


  while (1) {

    uint16_t quantity_ds18b20 = ds18b20_search_all();
    
    delay_us(2000000);
  }
}
