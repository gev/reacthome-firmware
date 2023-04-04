#include <stdint.h>
#include "gd32f3x0.h"

#define UNQ_ADDR_1 0x1FFFF7E0
#define UNQ_ADDR_2 0x1FFFF7AC
#define UNQ_ADDR_3 0x1FFFF7B0
#define UNQ_ADDR_4 0x1FFFF7B4

#define MAGIC_NUMBER 0xFFFFFFFFFFC5

volatile int64_t n1 = 0, n2 = 0, hash = 0;

int main() {
  n1 = *((uint32_t*)UNQ_ADDR_1);
	n1 <<= 32;
	n1 |= *((uint32_t*)UNQ_ADDR_2);


	n2 = *((uint32_t*)UNQ_ADDR_3);
	n2 <<= 32;
	n2 |= *((uint32_t*)UNQ_ADDR_4);

  hash = (n1 ^ n2) % MAGIC_NUMBER;

  while (1) {
  }
}

// volatile uint32_t unique[4] = {0};

// 	unique[0] = *((uint32_t*)0x1FFFF7E0);
// 	unique[1] = *((uint32_t*)0x1FFFF7AC);
// 	unique[2] = *((uint32_t*)0x1FFFF7B0);
// 	unique[3] = *((uint32_t*)0x1FFFF7B4);