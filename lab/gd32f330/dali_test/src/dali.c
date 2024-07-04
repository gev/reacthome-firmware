#include <stdint.h>

#include "delay.h"
#include "gd32f3x0.h"
#include "gd32f3x0_gpio.h"
#include "gd32f3x0_rcu.h"
#include "gpio.h"
#include "dali.h"

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



void dali_init(){
  rcu_periph_clock_enable(RCU_GPIOA);
  rcu_periph_clock_enable(RCU_GPIOB);
  delay_init();
  pin_init_input(dali_rx);
  pin_init_out(dali_tx);
  dali_pin_reset(dali_tx);
  delay_us(100000);
}

void set_search_addr(uint32_t addr) {
    uint8_t high = (addr >> 16) & 0xff;
    uint8_t middle = (addr >> 8) & 0xff;
    uint8_t low = addr & 0xff;

    dali_send_special_command(DALI_SEARCHADDRH, high);
		delay_ms(20);
    dali_send_special_command(DALI_SEARCHADDRM, middle);
		delay_ms(20);
    dali_send_special_command(DALI_SEARCHADDRL, low);
		delay_ms(20);

}



uint32_t find_next(uint32_t low, uint32_t high) {
    while (low <= high) {
        uint32_t mid = (low + high) / 2;

        set_search_addr(mid);
        dali_send_special_command(DALI_COMPARE, 0);
        bool response = !read_byte().isEmpty;

        if (response) {
            if (low == high) {
							  dali_send_special_command(DALI_WITHDRAW, 0);
								delay_ms(20);
                return mid;
            }

            high = mid;
        } else {
            low = mid + 1;
        }
    }

    return -1;
}



uint8_t find_ballasts(uint32_t* buf) {

    uint32_t num_ballasts = 0;
    dali_send_special_command(DALI_RESET, 0);
		delay_ms(20);
    dali_send_special_command(DALI_RESET, 0);
		delay_ms(120);
    dali_send_special_command(DALI_INITIALISE, 0); //repeat
    delay_ms(20);
    dali_send_special_command(DALI_INITIALISE, 0);
    delay_ms(20);
    dali_send_special_command(DALI_INITIALISE, 0);
    delay_ms(120);
    dali_send_special_command(DALI_RANDOMISE, 0); //repeat
    delay_ms(20);
    dali_send_special_command(DALI_RANDOMISE, 0); 
    delay_ms(120); // Randomise may take up to 100ms

    uint32_t low = 0;
    uint32_t high = 0xffffff;

    while (low != -1) {
        low = find_next(low, high);
        if (low != -1) {
            buf[num_ballasts] = low;
            num_ballasts++;
            low++;
        }
    }

    dali_send_special_command(DALI_TERMINATE, 0);
    return num_ballasts;
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


void dali_send_frame(uint8_t byte1, uint8_t byte2) {
  dali_send_bit(DALI_START_BIT);

  for(int i = 0; i < 8; i++) {
    dali_send_bit(((byte1 << i) & 0x80) >> 7);
  }
  for(int i = 0; i < 8; i++) {
    dali_send_bit(((byte2 << i) & 0x80) >> 7);
  }

  dali_send_bit(DALI_DOUBLE_STOP_BIT);
}

void dali_send_special_command(uint8_t command, uint8_t value) {
    dali_send_frame(command, value);
}


dali_recieve read_byte() {
  uint32_t count = 0; 
  dali_recieve result = {true, 0};

  while (dali_pin_get(dali_rx) == 0) {
    delay_us(1);
    count++;
    if (count > (DALI_TIME_BIT_US * 22)) {
      return result;
    }
  };

  delay_us(DALI_TIME_BIT_US + DALI_0_75_TIME_BIT_US);
  for (int i = 0; i < 8; i++) {
    if (dali_pin_get(dali_rx) == 0) {
      result.byte |= (0x80 >> i);
    }
    delay_us(DALI_TIME_BIT_US);
  }
	delay_us(DALI_TIME_BIT_US);
	delay_us(DALI_TIME_BIT_US);
  result.isEmpty = false;
  return result;
}