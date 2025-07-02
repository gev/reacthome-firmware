/*!
    \file    main.c
    \brief   SPI simplex communication use DMA

    \version 2023-12-31, V2.3.0, firmware for GD32F3x0
*/

/*
    Copyright (c) 2023, GigaDevice Semiconductor Inc.

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

#include "gd32f3x0.h"


void rcu_config(void);
void gpio_config(void);
void spi_config(void);
void cbm53_set_voltage(uint8_t ch, float v);

/*!
    \brief      main function
    \param[in]  none
    \param[out] none
    \retval     none
*/
int main(void)
{
    /* enable peripheral clock */
    rcu_config();
    /* configure GPIO */
    gpio_config();
    /* configure SPI */
    spi_config();

    spi_enable(SPI0);



    while(1) {
        for (uint32_t i = 0; i < 20000000; i++);
        cbm53_set_voltage(0, 0);
        cbm53_set_voltage(1, 0);
        for (uint32_t i = 0; i < 20000000; i++);
        cbm53_set_voltage(0, 1);
        cbm53_set_voltage(1, 1);
        for (uint32_t i = 0; i < 20000000; i++);
        cbm53_set_voltage(0, 2);
        cbm53_set_voltage(1, 2);
        for (uint32_t i = 0; i < 20000000; i++);
        cbm53_set_voltage(0, 3);
        cbm53_set_voltage(1, 3);
        for (uint32_t i = 0; i < 20000000; i++);
        cbm53_set_voltage(0, 3.3);
        cbm53_set_voltage(1, 3.3);

    }
}

void cbm53_set_voltage(uint8_t ch, float v){
    const uint8_t offset_ch = 14, offset_pd = 13, offset_ldak = 12;
    const float ref_voltage = 3.3;

    if (ch > 3 || v > ref_voltage) return;
    
    float value = 255/ref_voltage*v;
    uint16_t reg = 0;
    reg = (((uint16_t)value)) << 4 | ch << offset_ch | 1 << offset_pd | 0 << offset_ldak;
    gpio_bit_reset(GPIOA, GPIO_PIN_4);
    spi_i2s_data_transmit(SPI0, reg);
    while(spi_i2s_flag_get(SPI0, SPI_FLAG_TRANS));
    gpio_bit_set(GPIOA, GPIO_PIN_4);
}

/*!
    \brief      configure different peripheral clocks
    \param[in]  none
    \param[out] none
    \retval     none
*/
void rcu_config(void)
{
    rcu_periph_clock_enable(RCU_GPIOA);
    rcu_periph_clock_enable(RCU_GPIOB);
    rcu_periph_clock_enable(RCU_SPI0);
}

/*!
    \brief      configure the GPIO peripheral
    \param[in]  none
    \param[out] none
    \retval     none
*/
void gpio_config(void)
{
    gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_4);
    gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_4);
    gpio_bit_set(GPIOA, GPIO_PIN_4);

    gpio_af_set(GPIOB, GPIO_AF_0, GPIO_PIN_3 | GPIO_PIN_5);
    gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_3 | GPIO_PIN_5);
    gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_3 | GPIO_PIN_5);
}

void spi_config(void)
{
    spi_parameter_struct spi_init_struct;
    /* deinitilize SPI and the parameters */
    spi_i2s_deinit(SPI0);
    spi_struct_para_init(&spi_init_struct);

    spi_init_struct.trans_mode           = SPI_TRANSMODE_FULLDUPLEX;
    spi_init_struct.device_mode          = SPI_MASTER;
    spi_init_struct.frame_size           = SPI_FRAMESIZE_16BIT;
    spi_init_struct.clock_polarity_phase = SPI_CK_PL_HIGH_PH_1EDGE;
    spi_init_struct.nss                  = SPI_NSS_SOFT;
    spi_init_struct.prescale             = SPI_PSC_64;
    spi_init_struct.endian               = SPI_ENDIAN_MSB;
    spi_init(SPI0, &spi_init_struct);
}
