#include "gd32f4xx.h"
#include "stdint.h"


int main() {
  rcu_periph_clock_enable(RCU_GPIOC);

 /* configure USART Tx as alternate function push-pull */
  gpio_mode_set(GPIOC, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE,
                GPIO_PIN_10);
  gpio_output_options_set(GPIOC, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ,
                          GPIO_PIN_10);



  while (1) {
      gpio_bit_toggle(GPIOC, GPIO_PIN_10);
      for(uint32_t x = 0; x < 2000000; x ++);
  }
}
