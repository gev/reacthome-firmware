#include <stdint.h>

#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"

void init_pin_out(uint32_t port, uint32_t pin) { 
	gpio_mode_set(port, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, BIT(pin));
  gpio_output_options_set(port, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, BIT(pin));
	
}

int main() {
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);
  gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_PULLDOWN, GPIO_PIN_5);
  gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_5);
	gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_PULLDOWN, GPIO_PIN_7);
  gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_7);
  while (1) {
    gpio_bit_set(GPIOA, GPIO_PIN_5);
		for (uint32_t x = 0; x < 20000000; x++)
			;
    gpio_bit_reset(GPIOA, GPIO_PIN_5);
    for (uint32_t x = 0; x < 20000000; x++)
      ;
		gpio_bit_set(GPIOA, GPIO_PIN_7);
    for (uint32_t x = 0; x < 20000000; x++)
      ;
    gpio_bit_reset(GPIOA, GPIO_PIN_7);
    for (uint32_t x = 0; x < 20000000; x++)
      ;
  }
}

/*
	
*/