#include <stdint.h>

#include "gd32f3x0.h"

int main() {

	fmc_page_erase();

  fmc_unlock();
  fmc_word_program(0x8003000, 0x01020304);
  fmc_flag_clear(FMC_FLAG_END | FMC_FLAG_WPERR | FMC_FLAG_PGERR);
  fmc_lock();



  volatile uint32_t x;
	x = *((uint32_t *)0x8003000);
	
	while (1) {

	}
}
