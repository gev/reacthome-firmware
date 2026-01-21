#include <stdint.h>

#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"

uint8_t dmx_buffer[512] = {0};

void delay_us(uint32_t time){
  uint32_t start_tick = TIMER_CNT(TIMER1);
  while (TIMER_CNT(TIMER1) - start_tick < time);
}

void timer_config(void) {

  timer_parameter_struct timer_initpara;

  /* enable the peripherals clock */
  rcu_periph_clock_enable(RCU_TIMER1);

  /* deinit a TIMER */
  timer_deinit(TIMER1);
  /* initialize TIMER init parameter struct */
  timer_struct_para_init(&timer_initpara);
  /* TIMER1 configuration */
  timer_initpara.prescaler = 83;
  //   timer_initpara.alignedmode       = TIMER_COUNTER_EDGE;
  timer_initpara.counterdirection = TIMER_COUNTER_UP;
  timer_initpara.period = 0xFFFFFFFF;
  //   timer_initpara.clockdivision     = TIMER_CKDIV_DIV1;
  timer_init(TIMER1, &timer_initpara);
  timer_enable(TIMER1);
}

void uart_config(uint32_t baudrate) {
    rcu_periph_clock_enable(RCU_GPIOA);
    rcu_periph_clock_enable(RCU_USART1);
    usart_disable(USART1);

    gpio_af_set(GPIOA, GPIO_AF_1, GPIO_PIN_2 | GPIO_PIN_3);
    gpio_mode_set(GPIOA, GPIO_MODE_AF, GPIO_PUPD_PULLUP, GPIO_PIN_2 | GPIO_PIN_3);
    gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ,
                            GPIO_PIN_2 | GPIO_PIN_3);

    usart_deinit(USART1);
    usart_baudrate_set(USART1, baudrate);
    usart_word_length_set(USART1, USART_WL_8BIT);
    usart_stop_bit_set(USART1, USART_STB_2BIT);
    usart_parity_config(USART1, USART_PM_NONE);
    usart_transmit_config(USART1, USART_TRANSMIT_ENABLE);
    usart_receive_config(USART1, USART_RECEIVE_ENABLE);
    usart_enable(USART1);
}


void init_pin_out(uint32_t port, uint32_t pin) { 
	gpio_mode_set(port, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, pin);
  gpio_output_options_set(port, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, pin);
}

void init_pin_af(uint32_t port, uint32_t pin, uint32_t af) { 
  gpio_mode_set(port, GPIO_MODE_AF, GPIO_PUPD_PULLUP, pin);
  gpio_output_options_set(port, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, pin);
  gpio_af_set(port, af, pin);
}

void dmx_send_buf(uint8_t *buf, uint16_t len) {
    for (int i = 0; i < len; i++) {
        while (!usart_flag_get(USART1, USART_FLAG_TBE));
        usart_data_transmit(USART1, buf[i]);
    }
  while (!usart_flag_get(USART1, USART_FLAG_TC));
}

int main() {
  timer_config();
  uart_config(250000U);

  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);

  init_pin_out(GPIOA, GPIO_PIN_4); // rede
  gpio_bit_set(GPIOA, GPIO_PIN_4); // only transmit

  dmx_buffer[0] = 0;

  dmx_buffer[1] = 100;
  dmx_buffer[2] = 0;
  dmx_buffer[3] = 0;

  dmx_buffer[4] = 0;
  dmx_buffer[5] = 100;
  dmx_buffer[6] = 0;

  dmx_buffer[7] = 0;
  dmx_buffer[8] = 0;
  dmx_buffer[9] = 100;

  while (1) {
    
    // send break
    uart_config(100000U);
    uint8_t break_byte = 0;
    dmx_send_buf(&break_byte, 1);
    uart_config(250000U);

    dmx_send_buf(dmx_buffer, 512);

    delay_us(1000);
  }
}

/*
	
*/