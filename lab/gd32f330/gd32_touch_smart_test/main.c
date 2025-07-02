#include "gd32f3x0.h"                   
#include "gd32f3x0_gpio.h"              
#include "gd32f3x0_rcu.h"               
#include <stdint.h>
#include "sk6812.h"

#define TOUCH_NUMBER 4

void config_pullup(uint32_t port, uint32_t pin);

typedef struct {
	uint32_t port;
	uint32_t pin;
	uint8_t led_1;
	uint8_t led_2;
} touch_t;

touch_t touchs[TOUCH_NUMBER] = {{GPIOB, GPIO_PIN_7, 7, 8},
								{GPIOB, GPIO_PIN_5, 1, 2},
								{GPIOB, GPIO_PIN_8, 5, 6},
								{GPIOB, GPIO_PIN_4, 3, 4}};

int main(){
	SystemInit();
	rcu_periph_clock_enable(RCU_GPIOA);
	rcu_periph_clock_enable(RCU_GPIOB);

	gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_6);
	gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_6);
	
	gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_1);
	gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_1);


	for (uint8_t i = 0; i < TOUCH_NUMBER; i++){
		config_pullup(touchs[i].port, touchs[i].pin);
	}

	sk6812_initialize();
	while(1){
		for (uint8_t i = 0; i < TOUCH_NUMBER; i++){
			if (gpio_input_bit_get(touchs[i].port, touchs[i].pin)){
				sk6812_set(touchs[i].led_1, 0, 0, 0);
				sk6812_set(touchs[i].led_2, 0, 0, 0);
			} else {
				sk6812_set(touchs[i].led_1, 20, 0, 20);
				sk6812_set(touchs[i].led_2, 20, 0, 20);
			}
		}
	}
}

void config_pullup(uint32_t port, uint32_t pin){
	gpio_mode_set(port, GPIO_MODE_INPUT, GPIO_PUPD_PULLUP, pin);
	gpio_output_options_set(port, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, pin);
}