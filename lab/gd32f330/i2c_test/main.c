#include <stdint.h>

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


void gpio_config(void);
void i2c_config(void);
void i2c_transmit(uint8_t slave_addr, uint8_t *buf, uint16_t size);
void sht21_reset(void);
void sht21_reqest_termperature(void);

int main() {

  gpio_config();
  i2c_config();
  uint8_t temp[2] = {0};
  while (1) {
    sht21_reset();
    for(uint32_t x = 0; x < 1000000; x++);
    sht21_reqest_termperature();
    for(uint32_t x = 0; x < 1000000; x++);
    i2c_transmit(SHT21_ADDRESS, temp, sizeof(temp));
    for(uint32_t x = 0; x < 1000000; x++);
  }
}

void sht21_reset(void)
{
    uint8_t req = 0b11111110;
    i2c_transmit (SHT21_ADDRESS, &req, sizeof(req));
}
void sht21_reqest_termperature(void)
{
    uint8_t req = 0b11110011;
    i2c_transmit (SHT21_ADDRESS, &req, sizeof(req));
}

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
    // i2c_mode_addr_config(I2CX, I2C_I2CMODE_ENABLE, I2C_ADDFORMAT_7BITS, I2CX_OWN_ADDRESS);
    /* enable I2CX */
    i2c_enable(I2CX);
    /* enable acknowledge */
    i2c_ack_config(I2CX, I2C_ACK_ENABLE);
}

void i2c_transmit(uint8_t slave_addr, uint8_t *buf_tx, uint16_t size)
{
    /* wait until I2C bus is idle */
    while(i2c_flag_get(I2CX, I2C_FLAG_I2CBSY));
    /* send a start condition to I2C bus */
    i2c_start_on_bus(I2CX);
    /* wait until SBSEND bit is set */
    while(!i2c_flag_get(I2CX, I2C_FLAG_SBSEND));
    /* send slave address to I2C bus */
    i2c_master_addressing(I2CX, slave_addr, I2C_TRANSMITTER);
    /* wait until ADDSEND bit is set */
    while(!i2c_flag_get(I2CX, I2C_FLAG_ADDSEND));
    /* clear ADDSEND bit */
    i2c_flag_clear(I2CX, I2C_FLAG_ADDSEND);
    /* wait until the transmit data buffer is empty */
    while(!i2c_flag_get(I2CX, I2C_FLAG_TBE));

    for(uint16_t i = 0; i < size; i++) {
        /* data transmission */
        i2c_data_transmit(I2CX, buf_tx[i]);
        /* wait until the TBE bit is set */
        while(!i2c_flag_get(I2CX, I2C_FLAG_TBE));
    }
    /* send a stop condition to I2C bus */
    i2c_stop_on_bus(I2CX);
    while(I2C_CTL0(I2CX) & I2C_CTL0_STOP);
}

i2c_receive(uint8_t slave_addr, uint8_t *buf_rx, uint16_t size)
{
        /* wait until I2C bus is idle */
    while(i2c_flag_get(I2CX, I2C_FLAG_I2CBSY));
    /* send a start condition to I2C bus */
    i2c_start_on_bus(I2CX);
    /* wait until SBSEND bit is set */
    while(!i2c_flag_get(I2CX, I2C_FLAG_SBSEND));

        /* send slave address to I2C bus */
    i2c_master_addressing(I2CX, slave_addr, I2C_RECEIVER);
    /* wait until ADDSEND bit is set */
    while(!i2c_flag_get(I2CX, I2C_FLAG_ADDSEND));
    /* clear ADDSEND bit */
    i2c_flag_clear(I2CX, I2C_FLAG_ADDSEND);
    for(uint16_t i = 0; i < size; i++) {
        // if(13 == i) {
        //     /* wait until the second last data byte is received into the shift register */
        //     while(!i2c_flag_get(I2CX, I2C_FLAG_BTC));
        //     /* disable acknowledge */
        //     i2c_ack_config(I2CX, I2C_ACK_DISABLE);
        // }
        /* wait until the RBNE bit is set */
        while(!i2c_flag_get(I2CX, I2C_FLAG_RBNE));
        /* read a data from I2C_DATA */
        buf_rx[i] = i2c_data_receive(I2CX);
    }
}