#include <stdint.h>

#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"

#define FREQ 1000
#define FILL 50

#define TIME_SET (uint16_t)(1000000 / FREQ / (100 / FILL))
#define TIME_RESET (uint16_t)(1000000 / FREQ - TIME_SET)

uint16_t time_set = 0, time_reset = 0;

void fill(uint16_t x) {
  time_set = (((float)1000000) / ((float)FREQ) / ((float)100 / x));
  time_reset = (1000000 / FREQ - time_set);
}

typedef struct {
  uint32_t port;
  uint32_t num;
} pin;

pin dim_1 = {GPIOA, GPIO_PIN_9};
pin dim_4 = {GPIOA, GPIO_PIN_12};
pin rede = {GPIOA, GPIO_PIN_4};
pin dim_220v = {GPIOA, GPIO_PIN_2};
pin dim_220v_1 = {GPIOA, GPIO_PIN_6};

void pin_init_out(pin x) {
  gpio_mode_set(x.port, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, x.num);
  gpio_output_options_set(x.port, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, x.num);
}

void pin_toggle(pin x) { gpio_bit_toggle(x.port, x.num); }
void pin_set(pin x) { gpio_bit_set(x.port, x.num); }
void pin_reset(pin x) { gpio_bit_reset(x.port, x.num); }

void delay_us(uint32_t time) {
  uint32_t start_tick = TIMER_CNT(TIMER1);
  while (TIMER_CNT(TIMER1) - start_tick < time)
    ;
}
uint32_t x = 0;
void dim_few_sec(uint16_t f) {
  fill(f);
  while (x < 1000000) {
    pin_set(dim_4);
    delay_us(time_set);
    pin_reset(dim_4);
    delay_us(time_reset);
    x++;
  }
  x = 0;
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

void init_null_detect(void) {
  /* enable the key user clock */
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);
  rcu_periph_clock_enable(RCU_CFGCMP);

  /* configure button pin as input */
  gpio_mode_set(GPIOA, GPIO_MODE_INPUT, GPIO_PUPD_NONE, GPIO_PIN_5);

  /* enable and set key user EXTI interrupt to the lower priority */
  nvic_irq_enable(EXTI4_15_IRQn, 2U, 1U);

  /* connect key user EXTI line to key GPIO pin */
  syscfg_exti_line_config(EXTI_SOURCE_GPIOA, EXTI_SOURCE_PIN5);

  /* configure key user EXTI line */
  exti_init(EXTI_5, EXTI_INTERRUPT, EXTI_TRIG_RISING);
  exti_interrupt_flag_clear(EXTI_5);
}

uint8_t brighlest = 100; 
uint32_t time_pause = 9400;
uint8_t up_or_down = 0;

int main() {
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);
  timer_config();

  // pin_init_out(dim_1);
  // pin_init_out(dim_4);
  // pin_init_out(rede);
  // pin_reset(rede);

  pin_init_out(dim_220v_1);
  pin_reset(dim_220v_1);
  init_null_detect();
  
  // time_pause = 9400 / 100 * brighlest;

  while (1) {
    // pin_set(dim_4);
    // // delay_us(12);
    // delay_us(TIME_SET);
    // pin_reset(dim_4);
    // // delay_us(38);
    // delay_us(TIME_RESET);
  }
}



void EXTI4_15_IRQHandler() {
  if (RESET != exti_interrupt_flag_get(EXTI_5)) {
    delay_us(9500 - time_pause);
    pin_reset(dim_220v_1);
    delay_us(time_pause);
    pin_set(dim_220v_1);
    exti_interrupt_flag_clear(EXTI_5);
  }
}
    // delay_us(time_pause);
    // pin_set(dim_220v_1);
    // delay_us(9500 - time_pause);
    // pin_reset(dim_220v_1);
    // if (time_pause <= 20) up_or_down = 1;
    // if (time_pause >= 8900) up_or_down = 0;
    // if (up_or_down) {
    //   time_pause += 20;
    // } else {
    //   time_pause -= 20;
    // }
