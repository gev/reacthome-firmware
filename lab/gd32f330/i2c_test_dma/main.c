#include <stdint.h>
#include <string.h>

#include "gd32f3x0.h"

#define RCU_GPIO_I2C            RCU_GPIOA
#define RCU_I2C                 RCU_I2C0
#define I2C_SCL_PORT            GPIOA
#define I2C_SCL_PIN             GPIO_PIN_9
#define I2C_SDA_PORT            GPIOA
#define I2C_SDA_PIN             GPIO_PIN_10
#define I2CX                    I2C0
#define I2C_SPEED               100000
#define SHT21_ADDRESS           0x80
#define I2CX_OWN_ADDRESS        0x40

#define I2C_RECEIVE             0
#define I2C_TRANSMIT            1


void gpio_config(void);
void i2c_config(void);
void i2c_transmit_dma(uint8_t slave_addr, uint8_t *buf, uint16_t size);
void sht21_reset(void);
void sht21_request_termperature(void);
// void sht21_read_termperature(void);


int main(void)
{

    gpio_config();
    i2c_config();


    while(1) {
        sht21_reset();
        for(uint32_t i = 0; i < 1000000; i++);
        sht21_request_termperature();
        for(uint32_t i = 0; i < 1000000; i++);
        // sht21_read_termperature();
        // for(uint32_t i = 0; i < 1000000; i++);

    }
}

void sht21_reset(void)
{
    uint8_t req = 0b11111110;
    i2c_transmit_dma (SHT21_ADDRESS, &req, sizeof(req));
}

void sht21_request_termperature(void)
{
    uint8_t req = 0b11110011;
    i2c_transmit_dma (SHT21_ADDRESS, &req, sizeof(req));
}

// void sht21_read_termperature(){
//     uint8_t temperature[2];
//     i2c_receive(SHT21_ADDRESS, temperature, sizeof(temperature));
// }

void gpio_config(void)
{
    /* enable GPIO clock */
    rcu_periph_clock_enable(RCU_GPIO_I2C);

    /* connect I2C_SCL_GPIO_PIN to I2C_SCL */
    gpio_af_set(I2C_SCL_PORT, GPIO_AF_4, I2C_SCL_PIN);
    /* connect I2C_SDA_GPIO_PIN to I2C_SDA */
    gpio_af_set(I2C_SDA_PORT, GPIO_AF_4, I2C_SDA_PIN);
    /* configure GPIO pins of I2C */
    gpio_mode_set(I2C_SCL_PORT, GPIO_MODE_AF, GPIO_PUPD_PULLUP, I2C_SCL_PIN);
    gpio_output_options_set(I2C_SCL_PORT, GPIO_OTYPE_OD, GPIO_OSPEED_50MHZ, I2C_SCL_PIN);
    gpio_mode_set(I2C_SDA_PORT, GPIO_MODE_AF, GPIO_PUPD_PULLUP, I2C_SDA_PIN);
    gpio_output_options_set(I2C_SDA_PORT, GPIO_OTYPE_OD, GPIO_OSPEED_50MHZ, I2C_SDA_PIN);
}

/*!
    \brief      configure the I2CX interface
    \param[in]  none
    \param[out] none
    \retval     none
*/
void i2c_config(void)
{
    /* enable I2C clock */
    rcu_periph_clock_enable(RCU_I2C);
    /* configure I2C clock */
    i2c_clock_config(I2CX, I2C_SPEED, I2C_DTCY_2);
    /* configure I2C address */
    i2c_mode_addr_config(I2CX, I2C_I2CMODE_ENABLE, I2C_ADDFORMAT_7BITS, I2CX_OWN_ADDRESS);
    /* enable I2CX */
    i2c_enable(I2CX);
    /* enable acknowledge */
    i2c_ack_config(I2CX, I2C_ACK_ENABLE);
}


void i2c_nvic_config(void)
{
    nvic_priority_group_set(NVIC_PRIGROUP_PRE1_SUB3);
    nvic_irq_enable(I2C0_EV_IRQn, 0, 2);
    nvic_irq_enable(I2C0_ER_IRQn, 0, 1);
}

void i2c_transmit_dma(uint8_t addr_slave, uint8_t *buf, uint16_t size) { 
    dma_parameter_struct dma_init_struct;
    /* initialize DMA channel1 */
    rcu_periph_clock_enable(RCU_DMA);
    dma_struct_para_init (&dma_init_struct);

    dma_deinit(DMA_CH1);
    dma_init_struct.direction = DMA_MEMORY_TO_PERIPHERAL;
    dma_init_struct.memory_addr = (uint32_t)buf;
    dma_init_struct.memory_inc = DMA_MEMORY_INCREASE_ENABLE;
    dma_init_struct.memory_width = DMA_MEMORY_WIDTH_8BIT;
    dma_init_struct.number = size;
    dma_init_struct.periph_addr = (uint32_t)&I2C_DATA(I2C0);
    dma_init_struct.periph_inc = DMA_PERIPH_INCREASE_DISABLE;
    dma_init_struct.periph_width = DMA_PERIPHERAL_WIDTH_8BIT;
    dma_init_struct.priority = DMA_PRIORITY_HIGH;
    dma_init(DMA_CH1, &dma_init_struct);
    /* wait until I2C bus is idle */
    while(i2c_flag_get(I2C0, I2C_FLAG_I2CBSY));

    /* send a start condition to I2C bus */
    i2c_start_on_bus(I2C0);

     /* wait until SBSEND bit is set */
    while(!i2c_flag_get(I2C0, I2C_FLAG_SBSEND));

    /* send slave address to I2C bus*/
    i2c_master_addressing(I2C0, addr_slave, I2C_TRANSMITTER);

    /* wait until ADDSEND bit is set*/
    while(!i2c_flag_get(I2C0, I2C_FLAG_ADDSEND));
    /* clear ADDSEND bit */
    i2c_flag_clear(I2C0, I2C_FLAG_ADDSEND);

    /* enable I2C0 DMA */
    i2c_dma_config(I2C0, I2C_DMA_ON);

    /* enable DMA0 channel1 */
    dma_channel_enable(DMA_CH1);

    /* DMA0 channel5 full transfer finish flag */
    while(!dma_flag_get(DMA_CH1, DMA_FLAG_FTF));

    /* send a stop condition to I2C bus*/
    i2c_stop_on_bus(I2C0);

}