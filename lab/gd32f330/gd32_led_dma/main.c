#include "gd32f3x0.h"                   
#include "gd32f3x0_gpio.h"              
#include "gd32f3x0_rcu.h"               
#include <stdint.h>
#include "sk6812.h"

void timers_dma_pin_init(void){}


int main(){
	SystemInit();
	rcu_periph_clock_enable(RCU_GPIOA);
	rcu_periph_clock_enable(RCU_GPIOB);
	


	gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_4);  //rede
	gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_4);
	gpio_bit_reset(GPIOA, GPIO_PIN_4);

	gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_15);
	gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_15);
	
	timer_with_pin_init();
	
	while(1){

		for(uint32_t x = 0; x < 15000000; x++)
		;
	}
}




void timers_dma_pin_init(void){
	rcu_periph_clock_enable(RCU_GPIOA);
	rcu_periph_clock_enable(RCU_TIMER0);
	rcu_periph_clock_enable(RCU_TIMER15);

	gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_8); //LED
	gpio_af_set(GPIOB, GPIO_AF_2, GPIO_PIN_8);
	gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_8);

	timer_parameter_struct timer15_init = {	.prescaler = 101-1,  // 1.2 us
																					.alignedmode = TIMER_COUNTER_EDGE,
																					.counterdirection = TIMER_COUNTER_UP,
																					.period = 999,
																					.clockdivision = TIMER_CKDIV_DIV1,
																					.repetitioncounter = 0,
																			};
	timer_init(TIMER15, &timer15_init);

	timer_oc_parameter_struct timer15_init_out = {.outputstate = TIMER_CCX_DISABLE, 
																								.outputnstate = TIMER_CCXN_DISABLE,
																								.ocpolarity   = TIMER_OC_POLARITY_HIGH,
																								.ocnpolarity  = TIMER_OCN_POLARITY_HIGH,
																								.ocidlestate  = TIMER_OC_IDLE_STATE_LOW,
																								.ocnidlestate = TIMER_OCN_IDLE_STATE_LOW
																								};
	timer_channel_output_config(TIMER15, TIMER_CH_0, &timer15_init_out);

	/* channel configuration in PWM mode0*/
  timer_channel_output_pulse_value_config(TIMER15, TIMER_CH_0, 0);
  timer_channel_output_mode_config(TIMER15, TIMER_CH_0, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(TIMER15, TIMER_CH_0, TIMER_OC_SHADOW_ENABLE);
  timer_primary_output_config(TIMER15, ENABLE);

  /* send DMA requests at TIM_update event */
  timer_channel_dma_request_source_select(TIMER15, TIMER_DMAREQUEST_UPDATEEVENT);
  /* TIMERs update DMA request for capture compare on channel X */
  timer_dma_enable(TIMER15, TIMER_DMA_CH2D);

  /* shadow register for auto-reload preload enable */
  timer_auto_reload_shadow_enable(TIMER15);
  /* auto-reload preload enable */
  timer_enable(TIMER15);

	/* initialize DMA channel */
  dma_deinit(DMA_CH2);
  /* DMAx_CHx initialize */
	dma_parameter_struct dma_init_struct = {
																					.direction = DMA_MEMORY_TO_PERIPHERAL,
																					.memory_inc = DMA_MEMORY_INCREASE_ENABLE,
																					.memory_width = DMA_MEMORY_WIDTH_8BIT,
																					.periph_inc = DMA_PERIPH_INCREASE_DISABLE,
																					.periph_width = DMA_PERIPHERAL_WIDTH_16BIT,
																					.priority = DMA_PRIORITY_ULTRA_HIGH,
																				};
  dma_init(DMA_CH2, &dma_init_struct);

  /* update addresses of peripheral and memory */
  dma_periph_address_config(DMA_CH2, (uint32_t)&TIMER_CH0CV(TIMER15));


  /* DMAx_CHx mode configuration */
  dma_circulation_disable(DMA_CH2);
  dma_memory_to_memory_disable(DMA_CH2);


  /* enable DMAx_CHx transfer complete interrupt */
  dma_interrupt_enable(DMA_CH2, DMA_INT_FTF);

  /* enable NVIC for DMAx_CHx IRQ */
  nvic_irq_enable(DMA_Channel1_2_IRQn, 0, 0);
}