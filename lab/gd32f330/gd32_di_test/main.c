#include <stdint.h>

#include "gd32f3x0.h"
#include "gd32f3x0_rcu.h"
#include "gpio.h"
#include "sk6812.h"

extern pin di_1;
extern pin di_2;
extern pin di_3;
extern pin di_4;
extern pin di_5;
extern pin di_6;
extern pin di_7;
extern pin di_8;
extern pin di_9;
extern pin di_10;
extern pin di_11;
extern pin di_12;

int main() {
  SystemInit();
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);

  gpio_mode_set(GPIOB, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_8);
  gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_8);

  // gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_4);
  // gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_4);
  // gpio_bit_reset(GPIOA, GPIO_PIN_4);

  // gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_15);
  // gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_15);

	pin_init_input(di_1);
	pin_init_input(di_2);
	pin_init_input(di_3);
	pin_init_input(di_4);
	pin_init_input(di_5);
	pin_init_input(di_6);
	pin_init_input(di_7);
	pin_init_input(di_8);
	pin_init_input(di_9);
	pin_init_input(di_10);
	pin_init_input(di_11);
	pin_init_input(di_12);

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
    } else if (!pin_get(di_5)) {
      sk6812_set(1, 127, 127, 0);
    } else if (!pin_get(di_6)) {
      sk6812_set(1, 0, 127, 127);
    } else if (!pin_get(di_7)) {
      sk6812_set(1, 100, 100, 100);
    } else if (!pin_get(di_8)) {
      sk6812_set(1, 32, 49, 21);
    } else if (!pin_get(di_9)) {
      sk6812_set(1, 188, 47, 120);
    } else if (!pin_get(di_10)) {
      sk6812_set(1, 17, 61, 127);
    } else if (!pin_get(di_11)) {
      sk6812_set(1, 100, 115, 53);
    } else if (!pin_get(di_12)) {
      sk6812_set(1, 99, 250, 15);
    } else {
      sk6812_set(1, 0, 0, 0);
    }
  }
}
