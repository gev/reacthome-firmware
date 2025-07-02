#include "gd32f3x0.h"                   
#include "gd32f3x0_gpio.h"              
#include "gd32f3x0_rcu.h"               
#include <stdint.h>
#include "sk6812.h"


void config_output(uint32_t port, uint32_t pin);
void config_input(uint32_t port, uint32_t pin);
void start_measurent(uint32_t port, uint32_t pin);
void init_timer(void);
void init_inrterrupt_touch(void);

typedef struct {
	uint32_t touch_port;
	uint32_t touch_pin;
	uint32_t led_1;
	uint32_t led_2;
	uint32_t time;
	uint32_t calibrate;
} touch_t;

touch_t touch = {GPIOB, GPIO_PIN_8, 5, 6, 0, 0};

int main(){
	rcu_periph_clock_enable(RCU_GPIOA);
	rcu_periph_clock_enable(RCU_GPIOB);

	gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_6);
	gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_6);
	
	sk6812_initialize();

	config_output(touch.touch_port, touch.touch_pin);
	gpio_bit_reset(touch.touch_port, touch.touch_pin);


	init_timer();
	init_inrterrupt_touch();

	for (uint32_t x = 0; x < 20000000; x++);

	for (uint32_t i = 0; i < 1000; i++){
		start_measurent(touch.touch_port, touch.touch_pin);
		for (uint32_t x = 0; x < 2000; x++);
		if (touch.calibrate < touch.time){
			touch.calibrate = touch.time;
		}
	}

	while(1){

		for (uint32_t x = 0; x < 2000; x++);
		start_measurent(touch.touch_port, touch.touch_pin);
		for (uint32_t x = 0; x < 2000; x++);


		if (touch.time < touch.calibrate){
			sk6812_set(touch.led_1, 0, 0, 0);
			sk6812_set(touch.led_2, 0, 0, 0);
		} else {
			sk6812_set(touch.led_1, 200, 0, 0);
			sk6812_set(touch.led_2, 200, 0, 0);
		}

	}
}


void config_output(uint32_t port, uint32_t pin){
	gpio_mode_set(port, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, pin);
	gpio_output_options_set(port, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, pin);
}

void config_input(uint32_t port, uint32_t pin){
	gpio_mode_set(port, GPIO_MODE_INPUT, GPIO_PUPD_NONE, pin);
	gpio_output_options_set(port, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, pin);
}

void init_inrterrupt_touch(void) {
	/* enable the key user clock */
	rcu_periph_clock_enable(RCU_CFGCMP);
  
	/* configure button pin as input */
	gpio_mode_set(GPIOB, GPIO_MODE_INPUT, GPIO_PUPD_NONE, GPIO_PIN_8);
  
	/* enable and set key user EXTI interrupt to the lower priority */
	nvic_irq_enable(EXTI4_15_IRQn, 2U, 1U);
  
	/* connect key user EXTI line to key GPIO pin */
	syscfg_exti_line_config(EXTI_SOURCE_GPIOB, EXTI_SOURCE_PIN8);
  
	/* configure key user EXTI line */
	exti_init(EXTI_8, EXTI_INTERRUPT, EXTI_TRIG_RISING);
	exti_interrupt_flag_clear(EXTI_8);
  }

void EXTI4_15_IRQHandler() {
	if (RESET != exti_interrupt_flag_get(EXTI_8)) {
		exti_interrupt_flag_clear(EXTI_8);
		touch.time = TIMER_CNT(TIMER1);
		config_output(touch.touch_port, touch.touch_pin);
		gpio_bit_reset(touch.touch_port, touch.touch_pin);
	}
  }

  
  void start_measurent(uint32_t port, uint32_t pin){
	timer_counter_value_config(TIMER1, 0);
	config_input(port, pin);
  }

  void init_timer(void) {
  timer_parameter_struct timer_initpara;

  /* enable the peripherals clock */
  rcu_periph_clock_enable(RCU_TIMER1);

  /* deinit a TIMER */
  timer_deinit(TIMER1);
  /* initialize TIMER init parameter struct */
  timer_struct_para_init(&timer_initpara);
  /* TIMER1 configuration */
  timer_initpara.prescaler = 0;
  //   timer_initpara.alignedmode       = TIMER_COUNTER_EDGE;
  timer_initpara.counterdirection = TIMER_COUNTER_UP;
  timer_initpara.period = 0xFFFFFFFF;
  //   timer_initpara.clockdivision     = TIMER_CKDIV_DIV1;
  timer_init(TIMER1, &timer_initpara);
  timer_enable(TIMER1);
  }

