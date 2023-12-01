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
void i2c_nvic_config(void);
void i2c_transmit(uint8_t slave_addr, uint8_t *buf, uint16_t size);
void sht21_reset(void);
void sht21_request_termperature(void);
void sht21_read_termperature(void);

uint8_t i2c_buffer_transmitter[16];
uint8_t i2c_buffer_receiver[16];

volatile uint8_t* i2c_txbuffer;
volatile uint8_t* i2c_rxbuffer;
volatile uint16_t i2c_nbytes;
volatile uint16_t i2c_sent_bytes;
volatile uint16_t i2c_received_bytes;
volatile uint8_t i2c_flag_receive_transmit;
volatile uint8_t i2c_slave_addr;

int main(void)
{
    int i;

    gpio_config();
    i2c_config();
    /* configure the NVIC */
    i2c_nvic_config();


    /* initialize i2c_txbuffer, i2c_rxbuffer, i2c_nbytes and status */
    i2c_txbuffer = i2c_buffer_transmitter;
    i2c_rxbuffer = i2c_buffer_receiver;

    while(1) {
        sht21_reset();
        for(uint32_t i = 0; i < 1000000; i++);
        sht21_request_termperature();
        for(uint32_t i = 0; i < 1000000; i++);
        sht21_read_termperature();
        for(uint32_t i = 0; i < 1000000; i++);

    }
}

void sht21_reset(void)
{
    uint8_t req = 0b11111110;
    i2c_transmit (SHT21_ADDRESS, &req, sizeof(req));
}

void sht21_request_termperature(void)
{
    uint8_t req = 0b11110011;
    i2c_transmit (SHT21_ADDRESS, &req, sizeof(req));
}

void sht21_read_termperature(){
    uint8_t temperature[2];
    i2c_receive(SHT21_ADDRESS, temperature, sizeof(temperature));
}

void i2c_transmit(uint8_t slave_addr, uint8_t *buf, uint16_t size){
    memcpy (i2c_buffer_transmitter, buf, size);
    i2c_nbytes = size;
    i2c_slave_addr = slave_addr;
    i2c_sent_bytes = 0;
    i2c_flag_receive_transmit = I2C_TRANSMIT;

    /* enable the I2C0 interrupt */
    i2c_interrupt_enable(I2C0, I2C_INT_ERR);
    i2c_interrupt_enable(I2C0, I2C_INT_EV);
    i2c_interrupt_enable(I2C0, I2C_INT_BUF);

    /* the master waits until the I2C bus is idle */
    while(i2c_flag_get(I2C0, I2C_FLAG_I2CBSY));

    /* the master sends a start condition to I2C bus */
    i2c_start_on_bus(I2C0);
}

void i2c_receive(uint8_t slave_addr, uint8_t *buf, uint16_t size){
    i2c_nbytes = size;
    i2c_slave_addr = slave_addr;
    // i2c_received_bytes = 0;
    i2c_flag_receive_transmit = I2C_RECEIVE;

    i2c_ack_config(I2CX, I2C_ACK_ENABLE);
      /* enable the I2C0 interrupt */
    i2c_interrupt_enable(I2C0, I2C_INT_ERR);
    i2c_interrupt_enable(I2C0, I2C_INT_EV);
    i2c_interrupt_enable(I2C0, I2C_INT_BUF);

    /* the master waits until the I2C bus is idle */
    while(i2c_flag_get(I2C0, I2C_FLAG_I2CBSY));

    /* the master sends a start condition to I2C bus */
    i2c_start_on_bus(I2C0);
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
