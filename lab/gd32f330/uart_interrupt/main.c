#include <stdint.h>
#include "gd32f3x0.h"


#define ARRAYNUM(arr_nanme)      (uint32_t)(sizeof(arr_nanme) / sizeof(*(arr_nanme)))
#define TRANSMIT_SIZE   (ARRAYNUM(transmitter_buffer) - 1)

uint8_t transmitter_buffer[] = "\n\rUSART interrupt test\n\r";
uint8_t receiver_buffer[32];
uint8_t transfersize = TRANSMIT_SIZE;
uint8_t receivesize = 32;
__IO uint8_t txcount = 0;
__IO uint16_t rxcount = 0;

void usart0_gpio_config(void);
void usart0_config(void);

/*!
    \brief      main function
    \param[in]  none
    \param[out] none
    \retval     none
*/
int main(void)
{
    /* USART interrupt configuration */
    nvic_irq_enable(USART0_IRQn, 0, 0);

    /* initilize the com */
    usart0_gpio_config();
    usart0_config();

    /* enable USART TBE interrupt */
    usart_interrupt_enable(USART0, USART_INT_TBE);

    /* wait until USART send the transmitter_buffer */
    while(txcount < transfersize);

    while(RESET == usart_flag_get(USART0, USART_FLAG_TC));

    usart_interrupt_enable(USART0, USART_INT_RBNE);


    while(1);
}

/*!
    \brief      configure the USART0 GPIO ports
    \param[in]  none
    \param[out] none
    \retval     none
*/
void usart0_gpio_config(void)
{
    /* enable COM GPIO clock */
    rcu_periph_clock_enable(RCU_GPIOB);

    /* connect port to USARTx_Tx */
    gpio_af_set(GPIOB, GPIO_AF_0, GPIO_PIN_6);

    /* connect port to USARTx_Rx */
    gpio_af_set(GPIOB, GPIO_AF_0, GPIO_PIN_7);

    /* configure USART Tx as alternate function push-pull */
    gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_6);
    gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_10MHZ, GPIO_PIN_6);

    /* configure USART Rx as alternate function push-pull */
    gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_7);
    gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_10MHZ, GPIO_PIN_7);
}

/*!
    \brief      configure the USART0
    \param[in]  none
    \param[out] none
    \retval     none
*/
void usart0_config(void)
{
    /* enable USART clock */
    rcu_periph_clock_enable(RCU_USART0);

    /* USART configure */
    usart_deinit(USART0);
    usart_baudrate_set(USART0, 115200U);
    usart_receive_config(USART0, USART_RECEIVE_ENABLE);
    usart_transmit_config(USART0, USART_TRANSMIT_ENABLE);

    usart_enable(USART0);
}


void USART0_IRQHandler(void)
{
    if(RESET != usart_interrupt_flag_get(USART0, USART_INT_FLAG_RBNE)) {
        /* receive data */
        receiver_buffer[rxcount++] = usart_data_receive(USART0);
        if(rxcount == receivesize) {
            usart_interrupt_disable(USART0, USART_INT_RBNE);
        }
    }

    if(RESET != usart_interrupt_flag_get(USART0, USART_INT_FLAG_TBE)) {
        /* transmit data */
        usart_data_transmit(USART0, transmitter_buffer[txcount++]);
        if(txcount == transfersize) {
            usart_interrupt_disable(USART0, USART_INT_TBE);
        }
    }
}
