#ifndef _DS18B20_H_
#define _DS18B20_H_
#include <stdint.h>
#include "gd32f3x0.h"


#define DS18B20_NUMBER 256

typedef uint8_t ds18b20_rom_t [8];

uint16_t ds18b20_search_all(void);


#endif