#include "ds18b20.h"

#include <string.h>

#include "crc8.h"
#include "delay_us.h"
#include "one_wire.h"

typedef enum {
  STATE_INIT,
  STATE_SEARCH,
  STATE_READ_BIT,
  STATE_WRITE_BIT,
  STATE_CHECK_DEVICES,
  STATE_SUCCESS,
  STATE_FAILURE
} State;

State current_state;
uint8_t id_bit_number;
uint8_t last_zero;
uint8_t rom_byte_number;
uint8_t rom_byte_mask;
uint8_t search_result;

uint8_t saved_rom[8];
uint8_t last_discrepancy, last_family_discrepancy;
bool last_device_flag;

uint8_t id_bit, cmp_id_bit;
uint8_t search_direction;

bool ds18b20w_search_next(uint8_t* newAddr, uint8_t search_mode) {
  current_state = STATE_INIT;
  while (1) {
    switch (current_state) {
      case STATE_INIT:
        id_bit_number = 1;
        last_zero = 0;
        rom_byte_number = 0;
        rom_byte_mask = 1;
        search_result = 0;
        current_state = STATE_SEARCH;
        break;

      case STATE_SEARCH:
        if (last_device_flag){
          current_state = STATE_FAILURE;
          break;
        }
        if (!one_wire_reset()) {
          last_discrepancy = 0;
          last_device_flag = false;
          last_family_discrepancy = 0;
          current_state = STATE_FAILURE;
          break;
        }
        one_wire_write_byte(search_mode);
        current_state = STATE_READ_BIT;
        break;

      case STATE_READ_BIT:
        id_bit = one_wire_read_bit();
        cmp_id_bit = one_wire_read_bit();
        if ((id_bit == 1) && (cmp_id_bit == 1)) {
          // adfadf++;
          current_state = STATE_FAILURE;
        } else {
          current_state = STATE_WRITE_BIT;
        }
        break;

      case STATE_WRITE_BIT:
        if (id_bit != cmp_id_bit) {
          search_direction = id_bit;
        } else {
          if (id_bit_number < last_discrepancy) {
            search_direction =
                ((saved_rom[rom_byte_number] & rom_byte_mask) > 0);
          } else {
            search_direction = (id_bit_number == last_discrepancy);
          }
          if (search_direction == 0) {
            last_zero = id_bit_number;
            if (last_zero < 9) last_family_discrepancy = last_zero;
          }
        }
        if (search_direction == 1)
          saved_rom[rom_byte_number] |= rom_byte_mask;
        else
          saved_rom[rom_byte_number] &= ~rom_byte_mask;
        one_wire_write_bit(search_direction);
        id_bit_number++;
        rom_byte_mask <<= 1;
        if (rom_byte_mask == 0) {
          rom_byte_number++;
          rom_byte_mask = 1;
        }
        if (rom_byte_number == 8) {
          current_state = STATE_CHECK_DEVICES;
          break;
        }
        current_state = STATE_READ_BIT;
        break;

      case STATE_CHECK_DEVICES:
        if (id_bit_number > 64) {
          last_discrepancy = last_zero;
          if (last_discrepancy == 0) last_device_flag = true;
          search_result = true;
        }
        if(search_result || saved_rom[0] || (crc_ow(saved_rom, sizeof(saved_rom)) == 0) ){
          current_state = STATE_SUCCESS;
        } else {
          last_discrepancy = 0;
          last_device_flag = false;
          last_family_discrepancy = 0;
          search_result = false;
          current_state = STATE_FAILURE;
        }
        break;

      case STATE_SUCCESS:
        memcpy(newAddr, saved_rom, sizeof(saved_rom));
        return true;
        break;

      case STATE_FAILURE:
        return false;
        break;
    }
  }
}

ds18b20_rom_t ds18b20_pool[DS18B20_NUMBER];

uint16_t ds18b20_search_all() {
  uint16_t i = 0;
  ds18b20_rom_t ROM_NO = {0};
  last_discrepancy = 0;
  last_device_flag = false;
  last_family_discrepancy = 0;

  memset(saved_rom, 0, sizeof(saved_rom));
  memset(ds18b20_pool, 0, sizeof(ds18b20_pool));
  while (i < DS18B20_NUMBER) {
    bool found = ds18b20w_search_next(ROM_NO, SEARCH_ROM_NORMAL);

    if (!found) break;
    memcpy(ds18b20_pool[i], ROM_NO, sizeof(ROM_NO));
    i++;
  }
  return i;
}