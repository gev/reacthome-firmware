#include <stdint.h>

#include "delay.h"
#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"
#include "gpio.h"
#include "dali.h"


uint32_t ballasts_address[64] = {0};

int main() {

  dali_init();

  volatile uint8_t bls = find_ballasts(ballasts_address);
  delay_us(100);
  while (1) {

  }
}
