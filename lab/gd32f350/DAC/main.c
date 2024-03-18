#include <stdint.h>

#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"
#include "gd32f3x0_dac.h"

#define DAC_OUT_VAL0          2047



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
    gpio_mode_set(GPIOA, GPIO_MODE_ANALOG, GPIO_PUPD_NONE, GPIO_PIN_4);
}


void dac_config(void)
{
    dac_deinit();
    dac_trigger_disable();
    dac_wave_mode_config(DAC_WAVE_DISABLE);
    dac_enable();


    dac_output_buffer_enable();
    dac_data_set(DAC_ALIGN_12B_R, DAC_OUT_VAL0);
    dac_output_buffer_disable();

}
