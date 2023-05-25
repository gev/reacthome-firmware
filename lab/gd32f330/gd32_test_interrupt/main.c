#include <stdint.h>

#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"


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



void init_null_detect(void) {
  /* enable the key user clock */
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);
  rcu_periph_clock_enable(RCU_CFGCMP);

  /* configure button pin as input */
  gpio_mode_set(GPIOA, GPIO_MODE_INPUT, GPIO_PUPD_PULLUP, GPIO_PIN_5);

  /* enable and set key user EXTI interrupt to the lower priority */
  nvic_irq_enable(EXTI4_15_IRQn, 0U, 0U);

  /* connect key user EXTI line to key GPIO pin */
  syscfg_exti_line_config(EXTI_SOURCE_GPIOA, EXTI_SOURCE_PIN5);

  /* configure key user EXTI line */
  exti_init(EXTI_5, EXTI_INTERRUPT, EXTI_TRIG_RISING);
  exti_interrupt_flag_clear(EXTI_5);
}

// void init_null_detect(void) {
//   /* enable the key user clock */
//   rcu_periph_clock_enable(RCU_GPIOA);
//   rcu_periph_clock_enable(RCU_GPIOB);
//   rcu_periph_clock_enable(RCU_CFGCMP);

//   /* configure button pin as input */
//   gpio_mode_set(GPIOA, GPIO_MODE_INPUT, GPIO_PUPD_PULLUP, GPIO_PIN_10);

//   /* enable and set key user EXTI interrupt to the lower priority */
//   nvic_irq_enable(EXTI4_15_IRQn, 0U, 0U);

//   /* connect key user EXTI line to key GPIO pin */
//   syscfg_exti_line_config(EXTI_SOURCE_GPIOA, EXTI_SOURCE_PIN10);

//   /* configure key user EXTI line */
//   exti_init(EXTI_10, EXTI_INTERRUPT, EXTI_TRIG_RISING);
//   exti_interrupt_flag_clear(EXTI_10);
// }


int main() {
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);


  pin_init_out(dim_220v_1);
  pin_reset(dim_220v_1);
  init_null_detect();


  while (1) {

  }
}



void EXTI4_15_IRQHandler() {
  if (RESET != exti_interrupt_flag_get(EXTI_5)) {
    gpio_bit_toggle (GPIOA, GPIO_PIN_6);
    exti_interrupt_flag_clear(EXTI_5);
  }
}

// void EXTI4_15_IRQHandler() {
//   if (RESET != exti_interrupt_flag_get(EXTI_10)) {
//     gpio_bit_toggle (GPIOA, GPIO_PIN_6);
//     exti_interrupt_flag_clear(EXTI_10);
//   }
// }
