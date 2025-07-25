#include "gd32f3x0.h"                   
#include "gd32f3x0_gpio.h"              
#include "gd32f3x0_rcu.h"               
#include <stdint.h>
#include <stdio.h>


typedef struct {
	uint32_t port;
	uint32_t pin;
} pin_mcu_t;

void config_output(pin_mcu_t );
void config_input(pin_mcu_t );
void start_measurent(pin_mcu_t );
void pin_mcu_reset(pin_mcu_t );
void pin_mcu_set(pin_mcu_t );
void init_timer(void);
void init_inrterrupt_touch(void);


pin_mcu_t touch = {GPIOB, GPIO_PIN_8};
pin_mcu_t analizer = {GPIOA, GPIO_PIN_3};


int main(){
	rcu_periph_clock_enable(RCU_GPIOA);
	rcu_periph_clock_enable(RCU_GPIOB);
	

	config_output(touch);
	config_output(analizer);

	pin_mcu_reset(touch);
	config_output(touch);
	
	
	init_inrterrupt_touch();
	
	
	while(1){
		for (uint32_t x = 0; x < 30000; x++);
		config_input(touch);
		pin_mcu_set(analizer);
	}
}


void pin_mcu_reset(pin_mcu_t pin_mcu){
	gpio_bit_reset(pin_mcu.port, pin_mcu.pin);
}

void pin_mcu_set(pin_mcu_t pin_mcu){
	gpio_bit_set(pin_mcu.port, pin_mcu.pin);
}

void config_output(pin_mcu_t pin_mcu){
	gpio_mode_set(pin_mcu.port, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, pin_mcu.pin);
	gpio_output_options_set(pin_mcu.port, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, pin_mcu.pin);
}

void config_input(pin_mcu_t pin_mcu){
	gpio_mode_set(pin_mcu.port, GPIO_MODE_INPUT, GPIO_PUPD_NONE, pin_mcu.pin);
	gpio_output_options_set(pin_mcu.port, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, pin_mcu.pin);
}



void init_inrterrupt_touch(void) {
	rcu_periph_clock_enable(RCU_CFGCMP);
  
	gpio_mode_set(GPIOB, GPIO_MODE_INPUT, GPIO_PUPD_NONE, GPIO_PIN_8);
  
	nvic_irq_enable(EXTI4_15_IRQn, 2U, 1U);

	syscfg_exti_line_config(EXTI_SOURCE_GPIOB, EXTI_SOURCE_PIN8);
  
	exti_init(EXTI_8, EXTI_INTERRUPT, EXTI_TRIG_RISING);
	exti_interrupt_flag_clear(EXTI_8);
  }

void EXTI4_15_IRQHandler() {
	if (RESET != exti_interrupt_flag_get(EXTI_8)) {
		exti_interrupt_flag_clear(EXTI_8);
		config_output(touch);
		pin_mcu_reset(touch);

		pin_mcu_reset(analizer);

	}
  }
