#include "gd32f4xx.h"
#include "stdint.h"

// pc7 - rx, pc6 - tx, pd15 - rede
// usart5 AF8
// dma1 channel6 sub5
// pd12 - blink

void uart_init(void);
void dma_send_message(void);

uint8_t txbuffer[] = "Hello Zhenya";
uint8_t x;
int main() {
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);
  rcu_periph_clock_enable(RCU_GPIOC);
  rcu_periph_clock_enable(RCU_GPIOD);
  rcu_periph_clock_enable(RCU_GPIOE);

  gpio_mode_set(GPIOD, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_15);
  gpio_output_options_set(GPIOD, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_15);

  gpio_mode_set(GPIOD, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_12);
  gpio_output_options_set(GPIOD, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_12);

  uart_init();

  /* enable DMA1 */
  rcu_periph_clock_enable(RCU_DMA1);
  /* USART DMA enable for transmission and reception */
  usart_dma_transmit_config(USART5, USART_DENT_ENABLE);
  usart_dma_receive_config(USART5, USART_DENR_ENABLE);

  nvic_irq_enable(DMA1_Channel6_IRQn, 0, 0);

  while (1) {
    // gpio_bit_toggle(GPIOD, GPIO_PIN_12);
    // gpio_bit_toggle(GPIOD, GPIO_PIN_15);

    for (uint32_t x = 0; x < 10000000; x++)
      ;
    dma_send_message();
  }
}

void uart_init() {
  /* USART interrupt configuration */
  nvic_irq_enable(USART5_IRQn, 0, 0);

  /* enable USART clock */
  rcu_periph_clock_enable(RCU_USART5);

  /* connect port to USARTx_Tx */
  gpio_af_set(GPIOC, GPIO_AF_8, GPIO_PIN_6);

  /* connect port to USARTx_Rx */
  gpio_af_set(GPIOC, GPIO_AF_8, GPIO_PIN_7);

  /* configure USART Tx as alternate function push-pull */
  gpio_mode_set(GPIOC, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_6);
  gpio_output_options_set(GPIOC, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_6);

  /* configure USART Rx as alternate function push-pull */
  gpio_mode_set(GPIOC, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_7);
  gpio_output_options_set(GPIOC, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_7);

  /* USART configure */
  usart_deinit(USART5);
  usart_baudrate_set(USART5, 9600U);
  usart_receive_config(USART5, USART_RECEIVE_ENABLE);
  usart_transmit_config(USART5, USART_TRANSMIT_ENABLE);
  usart_enable(USART5);

  usart_interrupt_enable(USART5, USART_INT_RBNE);
}

void dma_send_message() {
  dma_single_data_parameter_struct dma_init_struct;

  /* enable DMA1 channel2 transfer complete interrupt */
  dma_interrupt_enable(DMA1, DMA_CH2, DMA_CHXCTL_FTFIE);
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

  /* enable DMA1 channel2 transfer complete interrupt */
  dma_interrupt_enable(DMA1, DMA_CH6, DMA_CHXCTL_FTFIE);
}

void DMA1_Channel6_IRQHandler(void) {
  if (dma_interrupt_flag_get(DMA1, DMA_CH6, DMA_INT_FLAG_FTF)) {
    dma_interrupt_flag_clear(DMA1, DMA_CH6, DMA_INT_FLAG_FTF);
    gpio_bit_toggle(GPIOD, GPIO_PIN_15);
  }
}

void USART5_IRQHandler(void) {
  if ((RESET != usart_interrupt_flag_get(USART5, USART_INT_FLAG_RBNE)) &&
      (RESET != usart_flag_get(USART5, USART_FLAG_RBNE))) {
    /* receive data */
    // rxbuffer[rxcount++] = usart_data_receive(USART5);
    // if(rxcount == rx_size) {
        // usart_interrupt_disable(USART5, USART_INT_RBNE);
    // }
    x = usart_data_receive(USART5);
    gpio_bit_toggle(GPIOD, GPIO_PIN_12);
  }
}