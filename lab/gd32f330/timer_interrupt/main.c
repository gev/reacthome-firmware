#include <stdint.h>
#include "gd32f3x0.h"


int main(){
	rcu_periph_clock_enable(RCU_GPIOA);
	rcu_periph_clock_enable(RCU_TIMER2);

  	gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_PULLUP, GPIO_PIN_8);
  	gpio_output_options_set(GPIOA, GPIO_OTYPE_OD, GPIO_OSPEED_MAX, GPIO_PIN_8);
  	gpio_bit_set(GPIOA, GPIO_PIN_8);

	nvic_irq_enable(TIMER2_IRQn, 0, 0);

	timer_parameter_struct timer_initpara;

    timer_deinit(TIMER2);

    timer_struct_para_init(&timer_initpara);

    timer_initpara.prescaler         = 83;
    timer_initpara.alignedmode       = TIMER_COUNTER_EDGE;
    timer_initpara.counterdirection  = TIMER_COUNTER_UP;
    timer_initpara.period            = 0xFF;
    timer_initpara.clockdivision     = TIMER_CKDIV_DIV1;
    timer_init(TIMER2, &timer_initpara);

    /* clear channel 0 interrupt bit */
    timer_interrupt_flag_clear(TIMER2, TIMER_INT_FLAG_UP);
    /* enable the TIMER interrupt */
    timer_interrupt_flag_clear(TIMER2, TIMER_INT_UP);
    /* enable a TIMER */
    // timer_interrupt_enable(TIMER2, TIMER_INT_UP);
    timer_enable(TIMER2);


	while(1){
		for (uint32_t i = 0; i < 2000000; i++)
		{}
		gpio_bit_reset(GPIOA, GPIO_PIN_8);
		timer_delay_us(4);
	}
}

void timer_delay_us(uint16_t time){
	timer_counter_value_config (TIMER2, 0xFF - (time - 1));
	timer_interrupt_flag_clear(TIMER2, TIMER_INT_UP);
	timer_interrupt_enable(TIMER2, TIMER_INT_UP);
}


void TIMER2_IRQHandler(void)
{
    if(SET == timer_interrupt_flag_get(TIMER2, TIMER_INT_UP)) {
        timer_interrupt_flag_clear(TIMER2, TIMER_INT_UP);
        timer_interrupt_disable(TIMER2, TIMER_INT_UP);
		gpio_bit_set(GPIOA, GPIO_PIN_8);

    }
    
}