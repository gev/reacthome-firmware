#include "ds18b20.h"

#include <string.h>

#include "delay_us.h"
#include "one_wire.h"

ds18b20_rom_t ds18b20_pool[DS18B20_NUMBER];

uint8_t saved_rom[8];
uint8_t last_discrepancy, last_family_discrepancy;
bool last_device_flag;

volatile uint8_t adfadf = 0;

typedef enum {
  Initialize,
  LastDevice,
  Reset,
  IssueCommand,
  SearchLoop,
  SearchSuccess,
  ResetFailed,
  Exit
} State;

bool ds18b20w_search_next(uint8_t* newAddr, uint8_t search_mode) {
  State state = Initialize;

  uint8_t id_bit_number;
  uint8_t last_zero, rom_byte_number, search_result;
  uint8_t id_bit, cmp_id_bit;

  uint8_t rom_byte_mask, search_direction;

  // initialize for search

  while (1) {
    switch (state) {
      case Initialize:
        // Initialize variables
        id_bit_number = 1;
        last_zero = 0;
        rom_byte_number = 0;
        rom_byte_mask = 1;
        search_result = 0;
        state = LastDevice;
        // Transition to the Reset state
        break;

      case LastDevice:
        if(last_device_flag) state = Exit;
        state = Reset;

      case Reset:
        // Perform a 1-Wire reset
        if (!one_wire_reset()) {
          // Reset failed
          // Transition to the ResetFailed state
          state = ResetFailed;
        } else {
          // Transition to the IssueCommand state
          state = IssueCommand;
        }
        break;

      case IssueCommand:
        // Issue the search command
        one_wire_write_byte(search_mode);
        // Transition to the SearchLoop state
        state = SearchLoop;
        break;

      case SearchLoop:
        // Read a bit and its complement
        id_bit = one_wire_read_bit();
        cmp_id_bit = one_wire_read_bit();

        if ((id_bit == 1) && (cmp_id_bit == 1)) {
          // No devices on 1-wire
          // Transition to the Exit state
          state = Exit;
        } else {
          if (id_bit != cmp_id_bit)
            search_direction = id_bit;
          else {
            if (id_bit_number < last_discrepancy)
              search_direction =
                  ((saved_rom[rom_byte_number] & rom_byte_mask) > 0);
            else
              search_direction = (id_bit_number == last_discrepancy);

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

          if (rom_byte_number < 8) {
            // Transition back to the SearchLoop state
            state = SearchLoop;
          } else {
            // Transition to the SearchSuccess state
            state = SearchSuccess;
          }
        }
        break;

      case SearchSuccess:
        last_discrepancy = last_zero;
        if (last_discrepancy == 0) last_device_flag = true;
        search_result = true;

        // Copy the ROM address to newAddr
        memcpy(newAddr, saved_rom, sizeof(saved_rom));

        // Transition to the Exit state
        state = Exit;
        break;

      case ResetFailed:
        // Reset variables
        last_discrepancy = 0;
        last_device_flag = false;
        last_family_discrepancy = 0;
        search_result = false;

        // Transition to the Exit state
        state = Exit;

      case Exit:
        return search_result;
    }
  }
}

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