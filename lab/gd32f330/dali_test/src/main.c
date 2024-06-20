#include <stdint.h>

#include "delay_us.h"
#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"
#include "gpio.h"

#define DALI_TIME_BIT_US  833 
#define DALI_HALF_TIME_BIT_US  (DALI_TIME_BIT_US / 2)

#define DALI_START_BIT        2
#define DALI_DOUBLE_STOP_BIT  3

#define dali_pin_reset pin_reset
#define dali_pin_set pin_set


pin dali_rx = {GPIOA, GPIO_PIN_0};
pin dali_tx = {GPIOA, GPIO_PIN_1};


void dali_send_frame(uint8_t, uint8_t);

int main() {
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);

  delay_init();

  pin_init_out(dali_tx);
  dali_pin_reset(dali_tx);



  while (1) {
    dali_send_frame(0xFE, 0x00);
    delay_us(2000000);
    dali_send_frame(0xFE, 0xf0);
    delay_us(2000000);
  }
}


void dali_send_bit(uint8_t bit){
  switch (bit) {
    case 0:
      dali_pin_reset(dali_tx);
      delay_us(DALI_HALF_TIME_BIT_US);
      dali_pin_set(dali_tx);
      delay_us(DALI_HALF_TIME_BIT_US);
      break;
    case 1:
      dali_pin_set(dali_tx);
      delay_us(DALI_HALF_TIME_BIT_US);
      dali_pin_reset(dali_tx);
      delay_us(DALI_HALF_TIME_BIT_US);
      break;
    case DALI_START_BIT:
      dali_pin_set(dali_tx);
      delay_us(DALI_HALF_TIME_BIT_US);
      dali_pin_reset(dali_tx);
      delay_us(DALI_HALF_TIME_BIT_US);
      break;
    case DALI_DOUBLE_STOP_BIT:
      dali_pin_reset(dali_tx);
      delay_us(DALI_TIME_BIT_US);
      delay_us(DALI_TIME_BIT_US);
      break;
  }
}

void dali_send_frame(uint8_t address, uint8_t data) {
  dali_send_bit(DALI_START_BIT);

  for(int i = 0; i < 8; i++) {
    dali_send_bit(((address << i) & 0x80) >> 7);
  }
  for(int i = 0; i < 8; i++) {
    dali_send_bit(((data << i) & 0x80) >> 7);
  }

  dali_send_bit(DALI_DOUBLE_STOP_BIT);
}