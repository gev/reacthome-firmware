
#include "gd32f4xx.h"
#include "src4392.h"
#include "string.h"

#define ARRAYSIZE 8

uint32_t i2s1_txbuffer[ARRAYSIZE] = {0xffff5555, 0x2222FFFF, 0x2222FFFF, 0x2222FFFF,
                                     0x0, 0xFFFFFFFF, 0x0, 0xFFFFFFFF};
uint32_t i2s1_rxbuffer[ARRAYSIZE] = {0};
uint32_t intermediate_buffer[ARRAYSIZE] = {0};

void dma_config(void);
void rcu_config(void);
void gpio_config(void);
void spi_config(void);


void swap_16bit_halves(uint32_t *buffer, size_t size) {
    for (size_t i = 0; i < size; i++) {
        buffer[i] = (buffer[i] >> 16) | (buffer[i] << 16);
    }
}

int main(void) {

  init_src4392();

  /* enable peripheral clock */
  rcu_config();
  /* configure GPIO */
  gpio_config();
  /* configure DMA */
  dma_config();
  /* configure I2S */
  spi_config();

  i2s_enable(SPI1);
  i2s_enable(I2S1_ADD);

  /* enable DMA channel */
  dma_channel_enable(DMA0, DMA_CH3);
  dma_channel_enable(DMA0, DMA_CH4);

  /* enable SPI DMA */
  spi_dma_enable(I2S1_ADD, SPI_DMA_RECEIVE);
  spi_dma_enable(SPI1, SPI_DMA_TRANSMIT);

  
  while (1) {
    while (!dma_flag_get(DMA0, DMA_CH4, DMA_FLAG_FTF));
    memcpy(intermediate_buffer, i2s1_rxbuffer, sizeof(intermediate_buffer));
    
    swap_16bit_halves(intermediate_buffer, ARRAYSIZE);

    for (uint32_t i = 0; i < ARRAYSIZE; i++) {
      // intermediate_buffer[i] ;
    }
    swap_16bit_halves(intermediate_buffer, ARRAYSIZE);

    memcpy(i2s1_txbuffer, intermediate_buffer, sizeof(i2s1_txbuffer));
    spi_dma_enable(SPI1, SPI_DMA_TRANSMIT);
  }
}

void rcu_config(void) {
  /* enable GPIOA clock */
  rcu_periph_clock_enable(RCU_GPIOA);
  /* enable GPIOB clock */
  rcu_periph_clock_enable(RCU_GPIOB);
  rcu_periph_clock_enable(RCU_GPIOC);
  rcu_periph_clock_enable(RCU_GPIOD);
  /* enable SPI1/I2S1 clock */
  rcu_periph_clock_enable(RCU_SPI1);
  /* enable SPI2/I2S2 clock */
  rcu_periph_clock_enable(RCU_SPI2);
  /* enable DMA0 clock */
  rcu_periph_clock_enable(RCU_DMA0);
  /* enable DMA1 clock */
  rcu_periph_clock_enable(RCU_DMA1);
}

/*!
    \brief      configure the GPIO peripheral
    \param[in]  none
    \param[out] none
    \retval     none
*/
void gpio_config(void) {
  /* configure I2S1 and I2S1_ADD pins: I2S1_WS(PD1), I2S1_CK(PC7),
   * I2S1_MCK(PC6), I2S1_SD(PB15), I2S1_ADD_SD(PB14) */
  gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_14 | GPIO_PIN_15);
  gpio_mode_set(GPIOD, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_1);
  gpio_mode_set(GPIOC, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_6 | GPIO_PIN_7);
  gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_25MHZ, GPIO_PIN_15 | GPIO_PIN_14);
  gpio_output_options_set(GPIOD, GPIO_OTYPE_PP, GPIO_OSPEED_25MHZ, GPIO_PIN_1);
  gpio_output_options_set(GPIOC, GPIO_OTYPE_PP, GPIO_OSPEED_25MHZ, GPIO_PIN_6 | GPIO_PIN_7); 
  gpio_af_set(GPIOB, GPIO_AF_6, GPIO_PIN_14);
  gpio_af_set(GPIOB, GPIO_AF_5, GPIO_PIN_15);
  gpio_af_set(GPIOD, GPIO_AF_7, GPIO_PIN_1);
  gpio_af_set(GPIOC, GPIO_AF_5, GPIO_PIN_6);
  gpio_af_set(GPIOC, GPIO_AF_5, GPIO_PIN_7);


 // shutdown off :
  gpio_mode_set(GPIOD, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_12);
  gpio_output_options_set(GPIOD, GPIO_OTYPE_PP, GPIO_OSPEED_25MHZ, GPIO_PIN_12);
  gpio_bit_set(GPIOD, GPIO_PIN_12);
  
}

/*!
    \brief      configure the SPI peripheral
    \param[in]  none
    \param[out] none
    \retval     none
*/
void spi_config(void) {

  spi_i2s_deinit(SPI1);


  /* configure I2S1 and I2S1_ADD */
  i2s_init(SPI1, I2S_MODE_MASTERTX, I2S_STD_PHILLIPS, I2S_CKPL_LOW);
  i2s_psc_config(SPI1, I2S_AUDIOSAMPLE_48K, I2S_FRAMEFORMAT_DT32B_CH32B,
                 I2S_MCKOUT_ENABLE);
  i2s_full_duplex_mode_config(I2S1_ADD, I2S_MODE_MASTERTX, I2S_STD_PHILLIPS,
                  I2S_CKPL_LOW, I2S_FRAMEFORMAT_DT32B_CH32B);
}

void dma_config(void) {
  dma_single_data_parameter_struct dma_init_struct;

  dma_single_data_para_struct_init(&dma_init_struct);
  /* configure SPI1 transmit dma */
  dma_deinit(DMA0, DMA_CH4);
  dma_init_struct.periph_addr = (uint32_t)&SPI_DATA(SPI1);
  dma_init_struct.memory0_addr = (uint32_t)i2s1_txbuffer;
  dma_init_struct.direction = DMA_MEMORY_TO_PERIPH;
  dma_init_struct.periph_memory_width = DMA_PERIPH_WIDTH_16BIT;
  dma_init_struct.priority = DMA_PRIORITY_LOW;
  dma_init_struct.number = ARRAYSIZE * 2;
  dma_init_struct.periph_inc = DMA_PERIPH_INCREASE_DISABLE;
  dma_init_struct.memory_inc = DMA_MEMORY_INCREASE_ENABLE;
  dma_init_struct.circular_mode = DMA_CIRCULAR_MODE_ENABLE;
  dma_single_data_mode_init(DMA0, DMA_CH4, &dma_init_struct);
  dma_channel_subperipheral_select(DMA0, DMA_CH4, DMA_SUBPERI0);
    
  /* configure I2S1_ADD receive dma */
  dma_deinit(DMA0, DMA_CH3);
  dma_init_struct.periph_addr = (uint32_t)&SPI_DATA(I2S1_ADD);
  dma_init_struct.memory0_addr = (uint32_t)i2s1_rxbuffer;
  dma_init_struct.direction = DMA_PERIPH_TO_MEMORY;
  dma_init_struct.priority = DMA_PRIORITY_HIGH;
  dma_single_data_mode_init(DMA0, DMA_CH3, &dma_init_struct);
  dma_channel_subperipheral_select(DMA0, DMA_CH3, DMA_SUBPERI3);
}