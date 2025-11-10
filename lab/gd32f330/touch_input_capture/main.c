#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>


void start_measurent();
void init_timer(void);
void uart_config(void);
float average(float, float);
void touch_reset();
void touch_af();

int _write(int file, char *ptr, int len);
void uart_stdout_init(void) { setvbuf(stdout, NULL, _IONBF, 0); }

void gpio_configuration(void) {
    rcu_periph_clock_enable(RCU_GPIOB);
    touch_reset();

}

int main() {
    rcu_periph_clock_enable(RCU_GPIOB);

    uart_config();
    uart_stdout_init();

    gpio_configuration();
    init_timer();

    
    while (1) {
        touch_af();
        timer_counter_value_config(TIMER2, 0);
        timer_flag_clear(TIMER2, TIMER_CH_2);
        while (!timer_flag_get(TIMER2, TIMER_CH_2)) {
        }
        touch_reset();
        uint16_t val = timer_channel_capture_value_register_read(TIMER2, TIMER_CH_2);
        printf("%u\n", val);
    }
}

void touch_reset(){
    gpio_mode_set(GPIOB, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_0);
    gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_0);
    gpio_bit_reset(GPIOB, GPIO_PIN_0);
}

void touch_af(){
    gpio_mode_set(GPIOB, GPIO_MODE_AF, GPIO_PUPD_NONE, GPIO_PIN_0);
    gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_0);

    gpio_af_set(GPIOB, GPIO_AF_1, GPIO_PIN_0);
}


void init_timer(void) {
    timer_parameter_struct timer_initpara;
    timer_ic_parameter_struct timer_icinitpara;

    /* enable the peripherals clock */
    rcu_periph_clock_enable(RCU_TIMER2);

    /* deinit a TIMER */
    timer_deinit(TIMER2);
    /* initialize TIMER init parameter struct */
    timer_struct_para_init(&timer_initpara);
    /* TIMER1 configuration */
    timer_initpara.prescaler = 0;
    //   timer_initpara.alignedmode       = TIMER_COUNTER_EDGE;
    timer_initpara.counterdirection = TIMER_COUNTER_UP;
    //   timer_initpara.clockdivision     = TIMER_CKDIV_DIV1;
    timer_init(TIMER2, &timer_initpara);

    timer_icinitpara.icpolarity = TIMER_IC_POLARITY_RISING;
    timer_icinitpara.icselection = TIMER_IC_SELECTION_DIRECTTI;
    timer_icinitpara.icprescaler = TIMER_IC_PSC_DIV1;
    timer_icinitpara.icfilter = 0x0;
    timer_input_capture_config(TIMER2, TIMER_CH_2, &timer_icinitpara);

    // timer_auto_reload_shadow_enable(TIMER2);

    timer_enable(TIMER2);
}

void uart_config(void) {
    rcu_periph_clock_enable(RCU_GPIOA);
    gpio_mode_set(GPIOA, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_2 | GPIO_PIN_3);
    gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ,
                            GPIO_PIN_2 | GPIO_PIN_3);
    gpio_af_set(GPIOA, GPIO_AF_1, GPIO_PIN_2 | GPIO_PIN_3);

    rcu_periph_clock_enable(RCU_USART1);

    usart_deinit(USART1);
    usart_baudrate_set(USART1, 115200U);
    usart_word_length_set(USART1, USART_WL_8BIT);
    usart_stop_bit_set(USART1, USART_STB_1BIT);
    usart_parity_config(USART1, USART_PM_NONE);
    usart_transmit_config(USART1, USART_TRANSMIT_ENABLE);
    usart_receive_config(USART1, USART_RECEIVE_ENABLE);
    usart_enable(USART1);
}

int _write(int file, char *ptr, int len) {
    int i;
    for (i = 0; i < len; i++) {
        /* отправить символ один за другим */
        usart_data_transmit(USART1, ptr[i]);
        while (!usart_flag_get(USART1, USART_FLAG_TBE))
        ;
    }
    return len;
}
