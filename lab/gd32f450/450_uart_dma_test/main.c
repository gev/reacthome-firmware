#include "gd32f4xx.h"
#include "stdint.h"

// pc7 - rx, pc6 - tx, pd15 - rede
// usart5 AF8
//dma1 channel6

void uart_init(void);
void dma_send_message(void);

uint8_t txbuffer[] = "Hello Zhenya";

int main() {
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);
  rcu_periph_clock_enable(RCU_GPIOC);
  rcu_periph_clock_enable(RCU_GPIOD);
  rcu_periph_clock_enable(RCU_GPIOE);

  uart_init();

  while (1) {
    for(uint32_t x = 0; x < 2000; x++);
    dma_send_message();
  }
}

void uart_init() {
  /* enable USART clock */
  rcu_periph_clock_enable(RCU_USART5);

  /* connect port to USARTx_Tx */
  gpio_af_set(GPIOC, GPIO_AF_8, GPIO_PIN_6);

  /* connect port to USARTx_Rx */
  gpio_af_set(GPIOC, GPIO_AF_8, GPIO_PIN_7);


  /* configure USART Tx as alternate function push-pull */
  gpio_mode_set(GPIOC, GPIO_MODE_AF, GPIO_PUPD_PULLUP,
                GPIO_PIN_6);
  gpio_output_options_set(GPIOC, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ,
                          GPIO_PIN_6);

  /* configure USART Rx as alternate function push-pull */
  gpio_mode_set(GPIOC, GPIO_MODE_AF, GPIO_PUPD_PULLUP,
                GPIO_PIN_7);
  gpio_output_options_set(GPIOC, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ,
                          GPIO_PIN_7);

  /* USART configure */
  usart_deinit(USART5);
  usart_baudrate_set(USART5, 1000000U);
  usart_receive_config(USART5, USART_RECEIVE_ENABLE);
  usart_transmit_config(USART5, USART_TRANSMIT_ENABLE);
  usart_enable(USART5);
}

void dma_send_message() {
  dma_single_data_parameter_struct dma_init_struct;
    /* enable DMA1 */
    rcu_periph_clock_enable(RCU_DMA1);

    /* deinitialize DMA channel7(USART0 TX) */
    dma_deinit(DMA1, DMA_CH6);
    dma_init_struct.direction = DMA_MEMORY_TO_PERIPH;
    dma_init_struct.memory0_addr = (uint32_t)txbuffer;
    dma_init_struct.memory_inc = DMA_MEMORY_INCREASE_ENABLE;
    dma_init_struct.periph_memory_width = DMA_PERIPH_WIDTH_8BIT;
    dma_init_struct.number = sizeof(txbuffer);
    dma_init_struct.periph_addr = (uint32_t)&USART_DATA(USART5);
    dma_init_struct.periph_inc = DMA_PERIPH_INCREASE_DISABLE;
    dma_init_struct.priority = DMA_PRIORITY_ULTRA_HIGH;
    dma_single_data_mode_init(DMA1, DMA_CH6, &dma_init_struct);
    /* configure DMA mode */
    dma_circulation_disable(DMA1, DMA_CH6);
    dma_channel_subperipheral_select(DMA1, DMA_CH6, DMA_SUBPERI5);
    /* enable DMA channel7 */
    dma_channel_enable(DMA1, DMA_CH6);

    /* USART DMA enable for transmission and reception */
    usart_dma_transmit_config(USART5, USART_DENT_ENABLE);
    usart_dma_receive_config(USART5, USART_DENR_ENABLE);

    /* wait DMA channel transfer complete */
    while(RESET == dma_flag_get(DMA1, DMA_CH6, DMA_FLAG_FTF));
}