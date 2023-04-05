
#include <stdio.h>
#include "gd32f3x0.h"

#define LED_NUM 5
#define LED_FOR_RST 7
#define LED_BUF_LENGTH (LED_NUM + LED_FOR_RST)

#define LED1 66
#define LED0 33

typedef struct{
	uint8_t g[8];
	uint8_t r[8];
	uint8_t b[8];
} led_t;

// #define TIMER0_CH0CC  ((uint32_t)0x40012c34)
led_t buffer[LED_BUF_LENGTH] = {0};

void gpio_config(void);
void timer_config(void);
void dma_config(void);

/*!
    \brief      configure the GPIO ports
    \param[in]  none
    \param[out] none
    \retval     none
  */
void gpio_config(void) {
  rcu_periph_clock_enable(RCU_GPIOB);

  /*configure PA8(TIMER0 CH0) as alternate function*/
  gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_8);
  gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_8);

  gpio_af_set(GPIOB, GPIO_AF_2, GPIO_PIN_8);
}

/*!
    \brief      configure the DMA peripheral
    \param[in]  none
    \param[out] none
    \retval     none
  */
void dma_config(void) {
  dma_parameter_struct dma_init_struct;

  /* enable DMA clock */
  rcu_periph_clock_enable(RCU_DMA);

  /* initialize DMA channel4 */
  dma_deinit(DMA_CH2);

  /* DMA channel4 initialize */
  dma_deinit(DMA_CH2);
  dma_init_struct.direction = DMA_MEMORY_TO_PERIPHERAL;
  dma_init_struct.memory_addr = (uint32_t)buffer;
  dma_init_struct.memory_inc = DMA_MEMORY_INCREASE_ENABLE;
  dma_init_struct.memory_width = DMA_MEMORY_WIDTH_8BIT;
  dma_init_struct.number = sizeof(buffer);
  dma_init_struct.periph_addr =
      (uint32_t)&TIMER_CH0CV(TIMER15);  //(uint32_t)TIMER0_CH0CC;
  dma_init_struct.periph_inc = DMA_PERIPH_INCREASE_DISABLE;
  dma_init_struct.periph_width = DMA_PERIPHERAL_WIDTH_16BIT;
  dma_init_struct.priority = DMA_PRIORITY_HIGH;
  dma_init(DMA_CH2, &dma_init_struct);

  /* configure DMA mode */
  dma_circulation_enable(DMA_CH2);
  dma_memory_to_memory_disable(DMA_CH2);

  /* enable DMA channel4 */
  dma_channel_enable(DMA_CH2);
}

/*!
    \brief      configure the TIMER peripheral
    \param[in]  none
    \param[out] none
    \retval     none
  */
void timer_config(void) {
  /* TIMER0 DMA Transfer example
  ------------------------------------------------- TIMER0CLK = 84MHz(GD32F330)
  or 108MHz(GD32F350), Prescaler = 84(GD32F330) or 108(GD32F350) TIMER0 counter
  clock 1MHz.

  the objective is to configure TIMER0 channel 1 to generate PWM
  signal with a frequency equal to 1KHz and a variable duty cycle(25%,50%,75%)
  that is changed by the DMA after a specific number of update DMA request.

  the number of this repetitive requests is defined by the TIMER0 repetition
  counter, each 2 update requests, the TIMER0 Channel 0 duty cycle changes to
  the next new value defined by the buffer .
  -----------------------------------------------------------------------------*/
  timer_oc_parameter_struct timer_ocintpara;
  timer_parameter_struct timer_initpara;

  rcu_periph_clock_enable(RCU_TIMER15);

  timer_deinit(TIMER15);

  /* TIMER0 configuration */
  timer_initpara.prescaler = 0;
  timer_initpara.alignedmode = TIMER_COUNTER_EDGE;
  timer_initpara.counterdirection = TIMER_COUNTER_UP;
  timer_initpara.period = 100;  // 999;
  timer_initpara.clockdivision = TIMER_CKDIV_DIV1;
  timer_initpara.repetitioncounter = 0;
  timer_init(TIMER15, &timer_initpara);

  /* CH0 configuration in PWM0 mode */
  timer_ocintpara.outputstate = TIMER_CCX_ENABLE;
  timer_ocintpara.outputnstate = TIMER_CCXN_ENABLE;
  timer_ocintpara.ocpolarity = TIMER_OC_POLARITY_HIGH;
  timer_ocintpara.ocnpolarity = TIMER_OCN_POLARITY_HIGH;
  timer_ocintpara.ocidlestate = TIMER_OC_IDLE_STATE_HIGH;
  timer_ocintpara.ocnidlestate = TIMER_OCN_IDLE_STATE_LOW;
  timer_channel_output_config(TIMER15, TIMER_CH_0, &timer_ocintpara);

  timer_channel_output_pulse_value_config(TIMER15, TIMER_CH_0, 0);
  timer_channel_output_mode_config(TIMER15, TIMER_CH_0, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(TIMER15, TIMER_CH_0,
                                     TIMER_OC_SHADOW_DISABLE);

  /* TIMER0 primary output enable */
  timer_primary_output_config(TIMER15, ENABLE);

  /* TIMER0 update DMA request enable */
  timer_dma_enable(TIMER15, TIMER_DMA_UPD);

  /* auto-reload preload enable */
  timer_auto_reload_shadow_enable(TIMER15);

  /* TIMER0 counter enable */
  timer_enable(TIMER15);
}



void set_color(uint8_t led, uint8_t r, uint8_t g, uint8_t b){
	for (uint8_t x = 0; x < 8; x++){
		if (r & 0x80) buffer[led].r[x] = LED1;
		else buffer[led].r[x] = LED0;
		r<<=1;
		if (g & 0x80) buffer[led].g[x] = LED1;
		else buffer[led].g[x] = LED0;
		g<<=1;
		if (b & 0x80) buffer[led].b[x] = LED1;
		else buffer[led].b[x] = LED0;
		b<<=1;
	}
} 

/*!
    \brief      main function
    \param[in]  none
    \param[out] none
    \retval     none
*/
int main(void) {
  gpio_config();
  dma_config();
  timer_config();

  while (1){
		set_color(0,100,100,100);
		set_color(1,0,0,100);
		set_color(2,100,0,0);
		set_color(3,0,100,0);
		set_color(4,100,0,100);
		for (uint32_t x = 0; x <10000000; x++);

		set_color(0,100,0,100);
		set_color(1,0,100,0);
		set_color(2,100,0,0);
		set_color(3,0,0,100);
	  set_color(4,100,100,100);
		for (uint32_t x = 0; x <10000000; x++);

	}
}
