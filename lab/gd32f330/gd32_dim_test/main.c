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
  time_set = (((float)1000000) / ((float) FREQ) / ((float)100 / x));
  time_reset = (1000000 / FREQ - time_set);
}

typedef struct {
  uint32_t port;
  uint32_t num;
} pin;

// pin relay[] = {
//     {GPIOA, GPIO_PIN_9},  // dim_test
// };
pin dim_1 = {GPIOA, GPIO_PIN_9};
pin dim_4 = {GPIOA, GPIO_PIN_12};
pin rede = {GPIOA, GPIO_PIN_4};

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

int main() {
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);
  timer_config();

  pin_init_out(dim_1);
  pin_init_out(dim_4);
  pin_init_out(rede);
  pin_reset(rede);

  while (1) {
    pin_set(dim_4);
    // delay_us(12);
    delay_us(TIME_SET);
    pin_reset(dim_4);
    // delay_us(38);
    delay_us(TIME_RESET);
  }
}
