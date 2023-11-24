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
#define I2CX_SLAVE_ADDRESS7     0x80

int main() {

  while (1) {

  }
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
    i2c_mode_addr_config(I2CX, I2C_I2CMODE_ENABLE, I2C_ADDFORMAT_7BITS, I2CX_SLAVE_ADDRESS7);
    /* enable I2CX */
    i2c_enable(I2CX);
    /* enable acknowledge */
    i2c_ack_config(I2CX, I2C_ACK_ENABLE);
}