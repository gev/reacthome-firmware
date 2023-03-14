#include "sk6812.h"

uint8_t colors[RGB_NUMBER * 3] = {0};

inline void delay_300ns() {
  asm(" nop");
  asm(" nop");
  asm(" nop");
}
inline void delay_900ns() {
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");

  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");

  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");

  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");

  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");

  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");

  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
  asm(" nop");
}

void sk6812_send_colors() {
  uint8_t byte;
  uint8_t bit;
  for (byte = 0; byte < RGB_NUMBER * 3; byte++) {
    for (bit = 0; bit < 8; bit++) {
      if (colors[byte] & (0x80 >> bit)) {
        leds_pin_write(1);
        delay_900ns();
        leds_pin_write(0);
        delay_300ns();
      } else {
        leds_pin_write(1);
        delay_300ns();
        leds_pin_write(0);
        asm(" nop");
        delay_900ns();
      }
    }
  }
}

void sk6812_set(uint8_t sequence, uint8_t r, uint8_t g, uint8_t b) {
  colors[((sequence - 1) * 3) + R_OFFSET] = r;
  colors[((sequence - 1) * 3) + G_OFFSET] = g;
  colors[((sequence - 1) * 3) + B_OFFSET] = b; 
  sk6812_send_colors();
  for(uint32_t x = 0; x < 10000; x++);
}

void sk6812_initialize(void) {
  uint32_t led;
  for (led = 0; led < RGB_NUMBER; led++) {
    sk6812_set(led, 0, 0, 0);
  }
  sk6812_send_colors();
}