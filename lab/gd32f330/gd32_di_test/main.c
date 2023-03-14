#include <stdint.h>

#include "gd32f3x0.h"
#include "gd32f3x0_rcu.h"
#include "gpio.h"
#include "sk6812.h"

extern pin di_1;
extern pin di_2;
extern pin di_3;
extern pin di_4;

int main() {
  SystemInit();
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);

  gpio_mode_set(GPIOB, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_8);
  gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_8);

  gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_4);
  gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_4);
  gpio_bit_reset(GPIOA, GPIO_PIN_4);

  gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_15);
  gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_15);

	pin_init_input(di_1);
	pin_init_input(di_2);
	pin_init_input(di_3);
	pin_init_input(di_4);

  sk6812_initialize();
  while (1) {
    if (!pin_get(di_1)) {
      sk6812_set(1, 255, 0, 0);
    } else if (!pin_get(di_2)) {
      sk6812_set(1, 0, 255, 0);
    } else if (!pin_get(di_3)) {
      sk6812_set(1, 0, 0, 255);
    } else if (!pin_get(di_4)) {
      sk6812_set(1, 127, 0, 127);
    } else {
      sk6812_set(1, 0, 0, 0);
    }
  }
}
