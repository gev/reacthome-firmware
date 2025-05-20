/*!
    \file    main.c
    \brief   master receiver and slave transmitter interrupt

    \version 2024-01-15, V3.2.0, firmware for GD32F4xx
*/

/*
    Copyright (c) 2024, GigaDevice Semiconductor Inc.

    Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this
       list of conditions and the following disclaimer.
    2. Redistributions in binary form must reproduce the above copyright notice,
       this list of conditions and the following disclaimer in the documentation
       and/or other materials provided with the distribution.
    3. Neither the name of the copyright holder nor the names of its contributors
       may be used to endorse or promote products derived from this software without
       specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
OF SUCH DAMAGE.
*/

#include "gd32f4xx.h"
#include <stdio.h>
#include "i2c2_ie.h"

uint8_t i2c_buffer_transmitter[16];
uint8_t i2c_buffer_receiver[16];
volatile uint8_t  *i2c_txbuffer;
volatile uint8_t  *i2c_rxbuffer;
volatile uint16_t i2c_nbytes;
volatile ErrStatus status;
ErrStatus state = ERROR;

void rcu_config(void);
void gpio_config(void);
void i2c_config(void);
void i2c_nvic_config(void);
ErrStatus memory_compare(uint8_t *src, uint8_t *dst, uint16_t length);

/*!
    \brief      main function
    \param[in]  none
    \param[out] none
    \retval     none
*/
int main(void)
{
    int i;
    /* configure RCU */
    rcu_config();
    /* configure GPIO */
    gpio_config();
    /* configure I2C */
    i2c_config();
    /* configure the NVIC */
    i2c_nvic_config();

    for(i = 0; i < 16; i++) {
        i2c_buffer_transmitter[i] = i + 0x80;
    }
    /* initialize i2c_txbuffer, i2c_rxbuffer, i2c_nbytes and status */
    i2c_txbuffer = i2c_buffer_transmitter;
    i2c_rxbuffer = i2c_buffer_receiver;
    i2c_nbytes = 16;
    status = ERROR;

    /* enable the I2C2 interrupt */
    i2c_interrupt_enable(I2C2, I2C_INT_ERR);
    i2c_interrupt_enable(I2C2, I2C_INT_EV);
    i2c_interrupt_enable(I2C2, I2C_INT_BUF);

    if(2 == i2c_nbytes) {
        /* send ACK for the next byte */
        i2c_ackpos_config(I2C2, I2C_ACKPOS_NEXT);
    }
    /* the master waits until the I2C bus is idle */
    while(i2c_flag_get(I2C2, I2C_FLAG_I2CBSY));
    /* the master sends a start condition to I2C bus */
    i2c_start_on_bus(I2C2);

    while(i2c_nbytes > 0);

    while(1) {
    }
}

/*!
    \brief      memory compare function
    \param[in]  src : source data
    \param[in]  dst : destination data
    \param[in]  length : the compare data length
    \param[out] none
    \retval     ErrStatus : ERROR or SUCCESS
*/
ErrStatus memory_compare(uint8_t *src, uint8_t *dst, uint16_t length)
{
    while(length--) {
        if(*src++ != *dst++) {
            return ERROR;
        }
    }
    return SUCCESS;
}

/*!
    \brief      enable the peripheral clock
    \param[in]  none
    \param[out] none
    \retval     none
*/
void rcu_config(void)
{
    /* enable GPIOB clock */
    rcu_periph_clock_enable(RCU_GPIOA);
    rcu_periph_clock_enable(RCU_GPIOC);
    /* enable I2C2 clock */
    rcu_periph_clock_enable(RCU_I2C2);
    /* enable I2C0 clock */
    rcu_periph_clock_enable(RCU_I2C0);
}

/*!
    \brief      configure the GPIO ports
    \param[in]  none
    \param[out] none
    \retval     none
*/
void gpio_config(void)
{

    /* connect PB10 to I2C2_SCL */
    gpio_af_set(GPIOA, GPIO_AF_4, GPIO_PIN_8);
    /* connect PB11 to I2C2_SDA */
    gpio_af_set(GPIOC, GPIO_AF_4, GPIO_PIN_9);

    /* configure GPIO pins of I2C2 */
    gpio_mode_set(GPIOA, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_8);
    gpio_output_options_set(GPIOA, GPIO_OTYPE_OD, GPIO_OSPEED_50MHZ, GPIO_PIN_8);
    gpio_mode_set(GPIOC, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_9);
    gpio_output_options_set(GPIOC, GPIO_OTYPE_OD, GPIO_OSPEED_50MHZ, GPIO_PIN_9);
}

/*!
    \brief      configure the I2C0 and I2C2 interfaces
    \param[in]  none
    \param[out] none
    \retval     none
*/
void i2c_config(void)
{
    /* I2C clock configure */
    i2c_clock_config(I2C2, 100000, I2C_DTCY_2);
    /* I2C address configure */
    i2c_mode_addr_config(I2C2, I2C_I2CMODE_ENABLE, I2C_ADDFORMAT_7BITS, I2C2_SLAVE_ADDRESS7);
    /* enable I2C2 */
    i2c_enable(I2C2);
    /* enable acknowledge */
    i2c_ack_config(I2C2, I2C_ACK_ENABLE);
}

/*!
    \brief      configure the NVIC peripheral
    \param[in]  none
    \param[out] none
    \retval     none
*/
void i2c_nvic_config(void)
{
    nvic_priority_group_set(NVIC_PRIGROUP_PRE1_SUB3);
    nvic_irq_enable(I2C2_EV_IRQn, 0, 4);
    nvic_irq_enable(I2C2_ER_IRQn, 0, 1);
}
