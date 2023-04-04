#include "gd32f3x0.h"                           
#include <stdint.h>


volatile uint32_t unique[4] = {0};

int main(){
	
	unique[0] = *((uint32_t*)0x1FFFF7E0);
	unique[1] = *((uint32_t*)0x1FFFF7AC);
	unique[2] = *((uint32_t*)0x1FFFF7B0);
	unique[3] = *((uint32_t*)0x1FFFF7B4);

	while(1){

	}
}
