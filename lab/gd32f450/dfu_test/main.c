#include "gd32f4xx.h"
#include "stdint.h"
#include "../../../support/inc/run_app_by_addr.h"


#define APP_OFFSET      0x8000u
#define APP_ADDRESS    (0x08000000u + APP_OFFSET)

// !!! this string must be called at the beginning of the main firmware!!!
// nvic_vector_table_set(NVIC_VECTTAB_FLASH, 0x8000u); 

void jump_to_app(void) {
  
  // typedef void (*app_entry_t)(void);
  // app_entry_t app_entry = (app_entry_t)(*(volatile uint32_t *)(APP_ADDRESS + 4u));
  
  __set_MSP(*(volatile uint32_t *)APP_ADDRESS);
  run_app_by_addr(APP_ADDRESS+4);
  // app_entry();
}

int main() {
  jump_to_app();

  while (1) {
  }
}
