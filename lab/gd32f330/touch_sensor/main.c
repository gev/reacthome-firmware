#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

typedef struct {
  uint32_t touch_port;
  uint32_t touch_pin;
  uint8_t irqn;
  uint32_t exti;
  uint8_t exti_source_port;
  uint8_t exti_source_pin;
  bool ready;
  float timestamp;
  float time;
} touch_t;

void config_output(touch_t *);
void config_input(touch_t *);
void start_measurent();
void init_timer(void);
void init_inrterrupt_touch(touch_t *);
void uart_config(void);
float average(float, float);
void handle_interrupt(void);

int _write(int file, char *ptr, int len);
void uart_stdout_init(void) { setvbuf(stdout, NULL, _IONBF, 0); }

touch_t touchs[] = {{GPIOB, GPIO_PIN_8, EXTI4_15_IRQn, EXTI_8,
                     EXTI_SOURCE_GPIOB, EXTI_SOURCE_PIN8, false, 0, 0},
                    {GPIOA, GPIO_PIN_15, EXTI4_15_IRQn, EXTI_15,
                     EXTI_SOURCE_GPIOA, EXTI_SOURCE_PIN15, false, 0, 0},
                    {GPIOB, GPIO_PIN_7, EXTI4_15_IRQn, EXTI_7,
                     EXTI_SOURCE_GPIOB, EXTI_SOURCE_PIN7, false, 0, 0},
                    {GPIOB, GPIO_PIN_3, EXTI2_3_IRQn, EXTI_3, EXTI_SOURCE_GPIOB,
                     EXTI_SOURCE_PIN3, false, 0, 0},
                    {GPIOB, GPIO_PIN_5, EXTI4_15_IRQn, EXTI_5,
                     EXTI_SOURCE_GPIOB, EXTI_SOURCE_PIN5, false, 0, 0},
                    {GPIOB, GPIO_PIN_4, EXTI4_15_IRQn, EXTI_4,
                     EXTI_SOURCE_GPIOB, EXTI_SOURCE_PIN4, false, 0, 0}};

// touch_pb8
// touch_pa15
// touch_pb7
// touch_pb3
// touch_pb5
// touch_pb4

uint16_t index = 0;
volatile bool ready;
touch_t *touch;
uint8_t counter = 0;
int main() {
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);

  uart_config();
  uart_stdout_init();

  init_timer();

  for (uint8_t i = 0; i < 6; i++) {
    config_output(&touchs[i]);
    init_inrterrupt_touch(&touchs[i]);
  }

  while (1) {
    touch = &touchs[index];

    uint32_t t0 = TIMER_CNT(TIMER1);
    start_measurent();
    while (ready == false)
      ;
    float time = touch->timestamp - t0;
    if (time > 0)
      touch->time = average(touch->time, time);
    config_output(touch);
    index += 1;
    index %= 6;
    // t0 = TIMER_CNT(TIMER1);
    // while (TIMER_CNT(TIMER1) - t0 < 5000)
    ;
    if (counter == 0) {
      printf("%u %u %u %u %u %u \n", (uint16_t)(touchs[0].time),
             (uint16_t)(touchs[1].time), (uint16_t)(touchs[2].time),
             (uint16_t)(touchs[3].time), (uint16_t)(touchs[4].time),
             (uint16_t)(touchs[5].time));
    }
	counter += 1;
  }
}

void config_output(touch_t *sensor) {
  gpio_mode_set(sensor->touch_port, GPIO_MODE_OUTPUT, GPIO_PUPD_PULLDOWN,
                sensor->touch_pin);
  gpio_output_options_set(sensor->touch_port, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ,
                          sensor->touch_pin);
  gpio_bit_reset(sensor->touch_port, sensor->touch_pin);
}

void config_input(touch_t *sensor) {
  gpio_mode_set(sensor->touch_port, GPIO_MODE_INPUT, GPIO_PUPD_NONE,
                sensor->touch_pin);
}

void init_inrterrupt_touch(touch_t *sensor) {
  /* enable the key user clock */
  rcu_periph_clock_enable(RCU_CFGCMP);

  /* configure button pin as input */
  // gpio_mode_set(sensor->touch_port, GPIO_MODE_INPUT, GPIO_PUPD_NONE,
  // sensor->touch_pin);

  /* enable and set key user EXTI interrupt to the lower priority */
  nvic_irq_enable(sensor->irqn, 0U, 0U);

  /* connect key user EXTI line to key GPIO pin */
  syscfg_exti_line_config(sensor->exti_source_port, sensor->exti_source_pin);

  /* configure key user EXTI line */
  exti_init(sensor->exti, EXTI_INTERRUPT, EXTI_TRIG_RISING);
  exti_interrupt_flag_clear(sensor->exti);
}

void EXTI4_15_IRQHandler() { handle_interrupt(); }

void EXTI2_3_IRQHandler() { handle_interrupt(); }

void handle_interrupt(void) {

  // if (RESET != exti_interrupt_flag_get(touchs[index].exti)) {
  exti_interrupt_flag_clear(touch->exti);
  touch->timestamp = TIMER_CNT(TIMER1);
  exti_interrupt_disable(touch->exti);
  ready = true;
  // average(&(touchs[i].time), (float)time);
  // config_output(touchs[i]);
  // }
}

void start_measurent() {
  ready = false;
  config_input(touch);
  exti_interrupt_enable(touch->exti);
}

void init_timer(void) {
  timer_parameter_struct timer_initpara;

  /* enable the peripherals clock */
  rcu_periph_clock_enable(RCU_TIMER1);

  /* deinit a TIMER */
  timer_deinit(TIMER1);
  /* initialize TIMER init parameter struct */
  timer_struct_para_init(&timer_initpara);
  /* TIMER1 configuration */
  timer_initpara.prescaler = 0;
  //   timer_initpara.alignedmode       = TIMER_COUNTER_EDGE;
  timer_initpara.counterdirection = TIMER_COUNTER_UP;
  //   timer_initpara.clockdivision     = TIMER_CKDIV_DIV1;
  timer_init(TIMER1, &timer_initpara);
  timer_enable(TIMER1);
}

void uart_config(void) {
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

float alpha = 0.01;
float average(float old_data, float new_data) {
  float value = old_data * (1 - alpha) + new_data * alpha;
  return value;
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
