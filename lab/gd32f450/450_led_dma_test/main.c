
#include <stdio.h>

#include "gd32f4xx.h"

#define LED_NUM 5
#define LENGHT_TRANSMIT (LED_NUM * 24 + 1)

#define LED1 90
#define LED0 30

typedef struct {
  uint16_t g[8];
  uint16_t r[8];
  uint16_t b[8];
} led_t;


led_t buffer[LED_NUM+1] = {0};

void timer_conf();

/*!
    \brief      configure the GPIO ports
    \param[in]  none
    \param[out] none
    \retval     none
*/
void gpio_config(void) {
  rcu_periph_clock_enable(RCU_GPIOB);
  rcu_periph_clock_enable(RCU_GPIOC);

  gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_0);  // ch2
  gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_0);
  gpio_af_set(GPIOB, GPIO_AF_2, GPIO_PIN_0);

  gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_5);  // ch1
  gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_5);
  gpio_af_set(GPIOB, GPIO_AF_2, GPIO_PIN_5);

  gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_4);  // ch0
  gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_4);
  gpio_af_set(GPIOB, GPIO_AF_2, GPIO_PIN_4);
}

/*!
    \brief      configure the DMA peripheral
    \param[in]  none
    \param[out] none
    \retval     none
*/
void dma_send_buf(uint16_t *buf, uint32_t size_buf, uint16_t timer_ch) {
  dma_single_data_parameter_struct dma_init_struct;

  /* enable DMA clock */
  rcu_periph_clock_enable(RCU_DMA0);

  /* initialize DMA channel5 */
  dma_deinit(DMA0, DMA_CH2);

  switch (timer_ch) {
    case TIMER_CH_0:
      dma_init_struct.periph_addr = (uint32_t)(&TIMER_CH0CV(TIMER2));
      break;

    case TIMER_CH_1:
      dma_init_struct.periph_addr = (uint32_t)(&TIMER_CH1CV(TIMER2));
      break;

    case TIMER_CH_2:
      dma_init_struct.periph_addr = (uint32_t)(&TIMER_CH2CV(TIMER2));
      break;

    case TIMER_CH_3:
      dma_init_struct.periph_addr = (uint32_t)(&TIMER_CH3CV(TIMER2));
      break;
  }
  // dma_init_struct.periph_addr =
  //     (uint32_t)(&TIMER_CH1CV(TIMER2));
  dma_init_struct.periph_inc = DMA_PERIPH_INCREASE_DISABLE;
  dma_init_struct.memory0_addr = (uint32_t)buf;
  dma_init_struct.memory_inc = DMA_MEMORY_INCREASE_ENABLE;
  dma_init_struct.periph_memory_width = DMA_PERIPH_WIDTH_16BIT;
  dma_init_struct.circular_mode = DMA_CIRCULAR_MODE_DISABLE;
  dma_init_struct.direction = DMA_MEMORY_TO_PERIPH;
  dma_init_struct.number = size_buf;
  dma_init_struct.priority = DMA_PRIORITY_ULTRA_HIGH;
  dma_single_data_mode_init(DMA0, DMA_CH2, &dma_init_struct);
  dma_channel_subperipheral_select(DMA0, DMA_CH2, DMA_SUBPERI5);

  /* enable DMA channel5 */
  dma_channel_enable(DMA0, DMA_CH2);

}

void timer_conf() {
  timer_oc_parameter_struct timer_ocintpara;
  timer_parameter_struct timer_initpara;

  rcu_periph_clock_enable(RCU_TIMER2);

  timer_deinit(TIMER2);

  /* TIMER2 configuration */
  timer_initpara.prescaler = 0;
  timer_initpara.alignedmode = TIMER_COUNTER_EDGE;
  timer_initpara.counterdirection = TIMER_COUNTER_UP;
  timer_initpara.period = 120;
  timer_initpara.clockdivision = TIMER_CKDIV_DIV1;
  timer_initpara.repetitioncounter = 0;
  timer_init(TIMER2, &timer_initpara);

  /* CH configuration in PWM0 mode */
  timer_ocintpara.outputstate = TIMER_CCX_ENABLE;
  timer_ocintpara.outputnstate = TIMER_CCXN_ENABLE;
  timer_ocintpara.ocpolarity = TIMER_OC_POLARITY_HIGH;
  timer_ocintpara.ocnpolarity = TIMER_OCN_POLARITY_HIGH;
  timer_ocintpara.ocidlestate = TIMER_OC_IDLE_STATE_HIGH;
  timer_ocintpara.ocnidlestate = TIMER_OCN_IDLE_STATE_LOW;
  timer_channel_output_config(TIMER2, TIMER_CH_0, &timer_ocintpara);
  timer_channel_output_config(TIMER2, TIMER_CH_1, &timer_ocintpara);
  timer_channel_output_config(TIMER2, TIMER_CH_2, &timer_ocintpara);
  timer_channel_output_config(TIMER2, TIMER_CH_3, &timer_ocintpara);

  timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_0, 0);
  timer_channel_output_mode_config(TIMER2, TIMER_CH_0, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(TIMER2, TIMER_CH_0, TIMER_OC_SHADOW_DISABLE);

  timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_1, 0);
  timer_channel_output_mode_config(TIMER2, TIMER_CH_1, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(TIMER2, TIMER_CH_1, TIMER_OC_SHADOW_DISABLE);

  timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_2, 0);
  timer_channel_output_mode_config(TIMER2, TIMER_CH_2, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(TIMER2, TIMER_CH_2, TIMER_OC_SHADOW_DISABLE);

  timer_channel_output_pulse_value_config(TIMER2, TIMER_CH_3, 0);
  timer_channel_output_mode_config(TIMER2, TIMER_CH_3, TIMER_OC_MODE_PWM0);
  timer_channel_output_shadow_config(TIMER2, TIMER_CH_3, TIMER_OC_SHADOW_DISABLE);




  timer_dma_enable(TIMER2, TIMER_DMA_UPD);

  /* auto-reload preload enable */
  timer_auto_reload_shadow_enable(TIMER2);

  /* TIMER2 counter enable */
  timer_enable(TIMER2);
}

void set_color(uint8_t led, uint8_t r, uint8_t g, uint8_t b) {
  for (uint8_t x = 0; x < 8; x++) {
    if (r & 0x80)
      buffer[led].r[x] = LED1;
    else
      buffer[led].r[x] = LED0;
    r <<= 1;
    if (g & 0x80)
      buffer[led].g[x] = LED1;
    else
      buffer[led].g[x] = LED0;
    g <<= 1;
    if (b & 0x80)
      buffer[led].b[x] = LED1;
    else
      buffer[led].b[x] = LED0;
    b <<= 1;
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
  timer_conf();

    for (uint32_t x = 0; x < 2000000; x++)
      ;

  set_color(0, 0, 100, 0);
  set_color(1, 0, 0, 100);
  set_color(2, 100, 0, 0);
  set_color(3, 0, 100, 0);
  set_color(4, 0, 0, 100);
  dma_send_buf((uint16_t *)buffer, LENGHT_TRANSMIT + 1, TIMER_CH_2);

    for (uint32_t x = 0; x < 2000000; x++)
    ;

  while (1) {
    set_color(0, 100, 100, 100);
    set_color(1, 0, 0, 100);
    set_color(2, 100, 0, 0);
    set_color(3, 0, 100, 0);
    set_color(4, 100, 0, 100);
    dma_send_buf((uint16_t *)buffer, LENGHT_TRANSMIT, TIMER_CH_0);
    set_color(0, 0, 100, 100);
    set_color(1, 0, 0, 100);
    set_color(2, 100, 100, 100);
    set_color(3, 0, 100, 0);
    set_color(4, 100, 0, 100);
    dma_send_buf((uint16_t *)buffer, LENGHT_TRANSMIT, TIMER_CH_1);
    for (uint32_t x = 0; x < 20000000; x++)
      ;

    set_color(0, 100, 0, 100);
    set_color(1, 0, 100, 0);
    set_color(2, 100, 0, 0);
    set_color(3, 0, 0, 100);
    set_color(4, 100, 100, 100);
    dma_send_buf((uint16_t *)buffer, LENGHT_TRANSMIT, TIMER_CH_0);
    set_color(0, 100, 0, 0);
    set_color(1, 0, 100, 100);
    set_color(2, 100, 100, 0);
    set_color(3, 0, 100, 0);
    set_color(4, 0, 0, 100);
    dma_send_buf((uint16_t *)buffer, LENGHT_TRANSMIT, TIMER_CH_1);
    for (uint32_t x = 0; x < 20000000; x++)
      ;
  }
}
