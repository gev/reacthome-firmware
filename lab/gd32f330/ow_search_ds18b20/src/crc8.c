#include "crc8.h"

uint8_t crc_ow(uint8_t* data, uint8_t size) {
  uint8_t crc = 0;
  for (uint8_t i = 0; i < size; i++) {
    uint8_t inbyte = data[i];
    for (uint8_t j = 0; j < 8; j++) {
      uint8_t mix = (crc ^ inbyte) & 0x01;
      crc >>= 1;
      if (mix) crc ^= 0x8C;
      inbyte >>= 1;
    }
  }
  return crc;
}
