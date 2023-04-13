#include <stdint.h>

#include "gd32f3x0.h"
#define PAGE_ADDR 0x8002000

int main() {
  // erase page
  fmc_unlock();
  fmc_flag_clear(FMC_FLAG_END | FMC_FLAG_WPERR | FMC_FLAG_PGERR);
  fmc_page_erase(PAGE_ADDR);
  fmc_flag_clear(FMC_FLAG_END | FMC_FLAG_WPERR | FMC_FLAG_PGERR);
  fmc_lock();

  // write 32 bit data
  fmc_unlock();
  fmc_flag_clear(FMC_FLAG_END | FMC_FLAG_WPERR | FMC_FLAG_PGERR);
  for (uint32_t x = 0; x < 10; x++) {
    fmc_word_program(PAGE_ADDR + (x * 4), 0x7);
    fmc_flag_clear(FMC_FLAG_END | FMC_FLAG_WPERR | FMC_FLAG_PGERR);
  }
  fmc_lock();

  volatile uint32_t x, y, t;
  x = *((uint32_t *)(PAGE_ADDR));
  y = *((uint32_t *)(PAGE_ADDR + 4));
  t = *((uint32_t *)(PAGE_ADDR + 8));
  x = x;

  while (1) {
  }
}
