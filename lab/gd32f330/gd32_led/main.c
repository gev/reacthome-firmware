#include "gd32f3x0.h"                   
#include "gd32f3x0_gpio.h"              
#include "gd32f3x0_rcu.h"               
#include <stdint.h>
#include "sk6812.h"

int main(){
	SystemInit();
	rcu_periph_clock_enable(RCU_GPIOA);
	rcu_periph_clock_enable(RCU_GPIOB);

	gpio_mode_set(GPIOB, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_8);
	gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_8);

	gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_4);
	gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_4);
	gpio_bit_reset(GPIOA, GPIO_PIN_4);

	gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_15);
	gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_15);

	sk6812_initialize();
	while(1){
			sk6812_set(1, 255, 255, 255);
		for(uint32_t x = 0; x < 15000000; x++)
		;
		sk6812_set(1, 0, 255, 255);
		for(uint32_t x = 0; x < 15000000; x++)
		;
		sk6812_set(1, 255, 0, 255);
		for(uint32_t x = 0; x < 15000000; x++)
		;
		sk6812_set(1, 255, 255, 0);
		for(uint32_t x = 0; x < 15000000; x++)
		;
		sk6812_set(1, 255, 0, 0);
		for(uint32_t x = 0; x < 15000000; x++)
		;
		sk6812_set(1, 0, 255, 0);
		for(uint32_t x = 0; x < 15000000; x++)
		;
		sk6812_set(1, 0, 0, 255);
		for(uint32_t x = 0; x < 15000000; x++)
		;
		sk6812_set(1, 0, 0, 0);
		for(uint32_t x = 0; x < 30000000; x++)
		;
		sk6812_set(1, 127, 127, 127);
		for(uint32_t x = 0; x < 15000000; x++)
		;
		sk6812_set(1, 0, 127, 127);
		for(uint32_t x = 0; x < 15000000; x++)
		;
		sk6812_set(1, 127, 0, 127);
		for(uint32_t x = 0; x < 15000000; x++)
		;
		sk6812_set(1, 127, 127, 0);
		for(uint32_t x = 0; x < 15000000; x++)
		;
		sk6812_set(1, 127, 0, 0);
		for(uint32_t x = 0; x < 15000000; x++)
		;
		sk6812_set(1, 0, 127, 0);
		for(uint32_t x = 0; x < 15000000; x++)
		;
		sk6812_set(1, 0, 0, 127);
		for(uint32_t x = 0; x < 15000000; x++)
		;
	}
}
