#include "gpio.h"

pin di_1 = {GPIOB, GPIO_PIN_4};
pin di_2 = {GPIOB, GPIO_PIN_7};
pin di_3 = {GPIOB, GPIO_PIN_6};
pin di_4 = {GPIOB, GPIO_PIN_5};
pin dim_1 = {GPIOA, GPIO_PIN_9};
pin dim_4 = {GPIOA, GPIO_PIN_12};
pin rede = {GPIOA, GPIO_PIN_4};
pin one_wire = {GPIOA, GPIO_PIN_15};

void pin_init_output(pin x) {
  gpio_mode_set(x.port, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, x.num);
  gpio_output_options_set(x.port, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, x.num);
}

void pin_init_output_OD(pin x) {
  gpio_mode_set(x.port, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, x.num);
  gpio_output_options_set(x.port, GPIO_OTYPE_OD, GPIO_OSPEED_MAX, x.num);
}

void pin_init_input(pin x) {
  gpio_mode_set(x.port, GPIO_MODE_INPUT, GPIO_PUPD_PULLUP, x.num);
}

void pin_toggle(pin x) { gpio_bit_toggle(x.port, x.num); }
void pin_set(pin x) { gpio_bit_set(x.port, x.num); }
void pin_reset(pin x) { gpio_bit_reset(x.port, x.num); }
bool pin_get(pin x) { return gpio_input_bit_get(x.port, x.num); }