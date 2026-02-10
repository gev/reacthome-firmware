#include <stdint.h>

#include "gd32f3x0.h"


int main() {
  nvic_vector_table_set(NVIC_VECTTAB_FLASH, 0x2000u);
  rcu_periph_clock_enable(RCU_GPIOA);

  gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_12);
  gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_12);

  while (1) {
    gpio_bit_set(GPIOA, GPIO_PIN_12);
		for (uint32_t x = 0; x < 20000000; x++)
			;
    gpio_bit_reset(GPIOA, GPIO_PIN_12);
    for (uint32_t x = 0; x < 20000000; x++)
      ;
  }
}

/*
	
*/