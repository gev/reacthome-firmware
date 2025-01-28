#include "src4392.h"
#include "gd32f3x0.h"

#define SRC_4392_ADDRESS 0b1110000
#define SRC_4392_CONFIG_N 10

uint8_t src_4392_config[SRC_4392_CONFIG_N][2] = {
    {0x7f, 0b00000000},
    {0x01, 0b00111111},
    {0x03, 0b00110001},
    {0x0d, 0b00001000},
    {0x0e, 0b00010000},
    {0x0f, 0b00010010},
    {0x10, 0b00000000},
    {0x11, 0b00000000},
    {0x2d, 0b00000010},
    {0x2f, 0b00000000},
};


i2c_config(){
    rcu_periph_clock_enable(RCU_GPIOA);
    rcu_periph_clock_enable(RCU_GPIOC);

    rcu_periph_clock_enable(RCU_I2C2);

    /* connect PB6 to I2C2_SCL */
    gpio_af_set(GPIOA, GPIO_AF_4, GPIO_PIN_8);
    /* connect PB7 to I2C2_SDA */
    gpio_af_set(GPIOC, GPIO_AF_4, GPIO_PIN_9);
    /* configure GPIO pins of I2C2 */
    gpio_mode_set(GPIOA, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_8);
    gpio_output_options_set(GPIOA, GPIO_OTYPE_OD, GPIO_OSPEED_50MHZ, GPIO_PIN_8);
    gpio_mode_set(GPIOC, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_9);
    gpio_output_options_set(GPIOC, GPIO_OTYPE_OD, GPIO_OSPEED_50MHZ, GPIO_PIN_9);

    /* configure I2C clock */
    i2c_clock_config(I2C2, 100000, I2C_DTCY_2);
    /* configure I2C address */
    // i2c_mode_addr_config(I2C2, I2C_I2CMODE_ENABLE, I2C_ADDFORMAT_7BITS, I2C2_OWN_ADDRESS7);
    /* enable I2C2 */
    i2c_enable(I2C2);
    /* enable acknowledge */
    i2c_ack_config(I2C2, I2C_ACK_ENABLE);

}

i2c_send(uint8_t addr, uint8_t *data, uint8_t size){
        /* wait until I2C bus is idle */
    while(i2c_flag_get(I2C2, I2C_FLAG_I2CBSY));
    /* send a start condition to I2C bus */
    i2c_start_on_bus(I2C2);
    /* wait until SBSEND bit is set */
    while(!i2c_flag_get(I2C2, I2C_FLAG_SBSEND));
    /* send slave address to I2C bus */
    i2c_master_addressing(I2C2, addr, I2C_TRANSMITTER);
    /* wait until ADDSEND bit is set */
    while(!i2c_flag_get(I2C2, I2C_FLAG_ADDSEND));
    /* clear ADDSEND bit */
    i2c_flag_clear(I2C2, I2C_FLAG_ADDSEND);
    /* wait until the transmit data buffer is empty */
    while(!i2c_flag_get(I2C2, I2C_FLAG_TBE));

    for(i = 0; i < size; i++) {
        /* data transmission */
        i2c_data_transmit(I2C2, data[i]);
        /* wait until the TBE bit is set */
        while(!i2c_flag_get(I2C2, I2C_FLAG_TBE));
    }
    /* send a stop condition to I2C bus */
    i2c_stop_on_bus(I2C2);
    while(I2C_CTL0(I2C2) & I2C_CTL0_STOP);
}

init_src4392(){
    i2c_config();
    for (uint8_t i = 0; i < SRC_4392_CONFIG_N; i++){
        i2c_send(SRC_4392_ADDRESS, src_4392_config[i], 2);
    }
}