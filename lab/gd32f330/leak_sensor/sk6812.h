#ifndef _SK6812_H_
#define _SK6812_H_

#include "gd32f3x0.h"                   
#include "gd32f3x0_gpio.h"              
#include "gd32f3x0_rcu.h"     

#define RGB_NUMBER  4

#define leds_pin_write(x)       x ? gpio_bit_set(GPIOB, GPIO_PIN_8) : gpio_bit_reset(GPIOB, GPIO_PIN_8) //SYS_PORTS_PinWrite(PORTS_ID_0, SK6812_PORT, SK6812_PIN, x)

#define  T0H_NS     300
#define  T0L_NS     900
#define  T1H_NS     900
#define  T1L_NS     300
#define  TRST_US    80

#define  R_OFFSET   1
#define  G_OFFSET   0
#define  B_OFFSET   2

void sk6812_set(uint8_t sequence, uint8_t r, uint8_t g, uint8_t b);
void sk6812_initialize(void);


#endif