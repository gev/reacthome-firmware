#include "gd32f4xx.h"
#include "stdint.h"

#define DAC_OUT_VAL0          400



void rcu_config(void);
void gpio_config(void);
void dac_config(void);

/*!
    \brief      main function
    \param[in]  none
    \param[out] none
    \retval     none
*/
int main(void)
{
    rcu_config();
    gpio_config();
    dac_config();
    while (1){

    }
}

void rcu_config(void)
{
    /* enable the clock of peripherals */
    rcu_periph_clock_enable(RCU_GPIOA);
    rcu_periph_clock_enable(RCU_DAC);
}

void gpio_config(void)
{
    /* once enabled the DAC, the corresponding GPIO pin is connected to the DAC converter automatically */
    gpio_mode_set(GPIOA, GPIO_MODE_ANALOG, GPIO_PUPD_NONE, GPIO_PIN_4 | GPIO_PIN_5);
}


void dac_config(void)
{
    dac_deinit();
    /* configure the DAC1 */
    dac_trigger_disable(DAC1);
    dac_wave_mode_config(DAC1, DAC_WAVE_DISABLE);
    dac_output_buffer_enable(DAC1);
    dac_enable(DAC1);
    dac_data_set(DAC1, DAC_ALIGN_12B_R, DAC_OUT_VAL0);
    dac_output_buffer_disable(DAC1);

}
