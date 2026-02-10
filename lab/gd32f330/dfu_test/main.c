#include <stdint.h>

#include "gd32f3x0.h"

#define APP_OFFSET      0x2000u
#define APP_ADDRESS    (0x08000000u + APP_OFFSET)

// !!! this string must be called at the beginning of the main firmware!!!
// nvic_vector_table_set(NVIC_VECTTAB_FLASH, 0x2000u); 

void jump_to_app(void) {
  
  typedef void (*app_entry_t)(void);
  app_entry_t app_entry = (app_entry_t)(*(volatile uint32_t *)(APP_ADDRESS + 4u));
  
  __set_MSP(*(volatile uint32_t *)APP_ADDRESS);

  app_entry();
}

int main() {
  rcu_periph_clock_enable(RCU_GPIOA);
  gpio_mode_set(GPIOA, GPIO_MODE_INPUT, GPIO_PUPD_NONE, GPIO_PIN_12);

  while (1) {
    if (gpio_input_bit_get(GPIOA, GPIO_PIN_12) == 0) {
      jump_to_app();
    }
  }
}
