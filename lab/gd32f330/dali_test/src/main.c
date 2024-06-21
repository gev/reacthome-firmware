#include <stdint.h>

#include "delay_us.h"
#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"
#include "gpio.h"

#define DALI_TIME_BIT_US       833 
#define DALI_HALF_TIME_BIT_US  (DALI_TIME_BIT_US / 2)
#define DALI_0_75_TIME_BIT_US  (DALI_TIME_BIT_US * 3 / 4)

#define DALI_START_BIT        2
#define DALI_DOUBLE_STOP_BIT  3

#define dali_pin_reset pin_reset
#define dali_pin_set   pin_set
#define dali_pin_get   pin_get


pin dali_rx = {GPIOA, GPIO_PIN_0};
pin dali_tx = {GPIOA, GPIO_PIN_1};

uint8_t byte_read = 0;
 
uint8_t read_byte() {
  while (dali_pin_get(dali_rx) == 0);
  delay_us(DALI_TIME_BIT_US + DALI_0_75_TIME_BIT_US);
  uint8_t result = 0;
  for (int i = 0; i < 8; i++) {
    if (dali_pin_get(dali_rx) == 0) {
      result |= (0x80 >> i);
    }
    delay_us(DALI_TIME_BIT_US);
  }
  return result;
}

void dali_send_frame(uint8_t, uint8_t);
volatile uint8_t res;

int main() {
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);

  delay_init();

  pin_init_out(dali_tx);
  dali_pin_reset(dali_tx);



  while (1) {
    dali_send_frame(0xFE, 0xf0);
    delay_us(2000000);
    dali_send_frame(0xFF, 0x91);
    res = 0;
    res = read_byte();
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
void dali_send_double_frame(uint8_t address, uint8_t byte1, uint8_t byte2) {
  dali_send_bit(DALI_START_BIT);

  for(int i = 0; i < 8; i++) {
    dali_send_bit(((address << i) & 0x80) >> 7);
  }
  for(int i = 0; i < 8; i++) {
    dali_send_bit(((byte1 << i) & 0x80) >> 7);
  }
  for(int i = 0; i < 8; i++) {
    dali_send_bit(((byte2 << i) & 0x80) >> 7);
  }

  dali_send_bit(DALI_DOUBLE_STOP_BIT);
}