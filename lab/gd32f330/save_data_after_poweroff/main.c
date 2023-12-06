#include <stdint.h>

#include "gd32f3x0.h"
#include "sk6812.h"

#define PAGE_ADDR 0x800fc00
uint32_t flash_data;

void leds_on(void);
void set_pin_output(uint32_t port, uint32_t pin);

int main(){
  flash_data = *((uint32_t*)PAGE_ADDR) + 0x01010101;
  rcu_periph_clock_enable(RCU_GPIOA);

	gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_11);
	gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_11);
  gpio_bit_reset(GPIOA, GPIO_PIN_11);

  set_pin_output(GPIOA, GPIO_PIN_0);
  set_pin_output(GPIOA, GPIO_PIN_1);
  set_pin_output(GPIOA, GPIO_PIN_2);
  set_pin_output(GPIOA, GPIO_PIN_3);
  set_pin_output(GPIOA, GPIO_PIN_6);
  set_pin_output(GPIOA, GPIO_PIN_7);
  set_pin_output(GPIOA, GPIO_PIN_8);
  set_pin_output(GPIOA, GPIO_PIN_9);
  set_pin_output(GPIOA, GPIO_PIN_10);
  set_pin_output(GPIOA, GPIO_PIN_11);
  set_pin_output(GPIOB, GPIO_PIN_0);
  set_pin_output(GPIOB, GPIO_PIN_1);
  
  /* configure NVIC */
  nvic_priority_group_set(NVIC_PRIGROUP_PRE0_SUB4);
  nvic_irq_enable(LVD_IRQn, 0, 0);
  /* enable clock */
  rcu_periph_clock_enable(RCU_PMU);

  exti_interrupt_flag_clear(EXTI_16);
  /* configure EXTI_16  */
  exti_init(EXTI_16, EXTI_INTERRUPT, EXTI_TRIG_BOTH);
  /* configure the lvd threshold to 3.1v */
  pmu_lvd_select(PMU_LVDT_7);

  leds_on();



  while(1){

    
  }
}

/*!
    \brief      this function handles LVD exception
    \param[in]  none
    \param[out] none
    \retval     none
*/
void LVD_IRQHandler(void)
{
    if(RESET != exti_interrupt_flag_get(EXTI_16)) {
        exti_interrupt_flag_clear(EXTI_16);

      // gpio_bit_set(GPIOA, GPIO_PIN_11);

      gpio_deinit(GPIOA);
      gpio_deinit(GPIOB);

       // erase page
      fmc_unlock();
      fmc_flag_clear(FMC_FLAG_END | FMC_FLAG_WPERR | FMC_FLAG_PGERR);
      fmc_page_erase(PAGE_ADDR);
      fmc_flag_clear(FMC_FLAG_END | FMC_FLAG_WPERR | FMC_FLAG_PGERR);
      fmc_lock();
          
          // write 32 bit data
      fmc_unlock();
      fmc_flag_clear(FMC_FLAG_END | FMC_FLAG_WPERR | FMC_FLAG_PGERR);
      for (uint32_t x = 0; x < 200; x++) {
        fmc_word_program(PAGE_ADDR + (x * 4), flash_data);
        fmc_flag_clear(FMC_FLAG_END | FMC_FLAG_WPERR | FMC_FLAG_PGERR);
      }
      fmc_lock();
    }
}

void leds_on(void){
  rcu_periph_clock_enable(RCU_GPIOA);
	rcu_periph_clock_enable(RCU_GPIOB);

	gpio_mode_set(GPIOB, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_8);
	gpio_output_options_set(GPIOB, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_8);

	gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_4);
	gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_4);
	gpio_bit_reset(GPIOA, GPIO_PIN_4);

	gpio_mode_set(GPIOA, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO_PIN_15);
	gpio_output_options_set(GPIOA, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, GPIO_PIN_15);

	sk6812_initialize();
  for (uint32_t x = 0; x < 20; x++) {
    sk6812_set(x, 255, 255, 255);
  }


}


void set_pin_output(uint32_t port, uint32_t pin){
  gpio_mode_set(port, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, pin);
  gpio_output_options_set(port, GPIO_OTYPE_PP, GPIO_OSPEED_50MHZ, pin);
  gpio_bit_set(port, pin);
}

// #define PAGE_ADDR 0x8002000

// int main() {
//   // erase page
//   fmc_unlock();
//   fmc_flag_clear(FMC_FLAG_END | FMC_FLAG_WPERR | FMC_FLAG_PGERR);
//   fmc_page_erase(PAGE_ADDR);
//   fmc_flag_clear(FMC_FLAG_END | FMC_FLAG_WPERR | FMC_FLAG_PGERR);
//   fmc_lock();

//   // write 32 bit data
//   fmc_unlock();
//   fmc_flag_clear(FMC_FLAG_END | FMC_FLAG_WPERR | FMC_FLAG_PGERR);
//   for (uint32_t x = 0; x < 10; x++) {
//     fmc_word_program(PAGE_ADDR + (x * 4), 0x7);
//     fmc_flag_clear(FMC_FLAG_END | FMC_FLAG_WPERR | FMC_FLAG_PGERR);
//   }
//   fmc_lock();

//   volatile uint32_t x, y, t;
//   x = *((uint32_t *)(PAGE_ADDR));
//   y = *((uint32_t *)(PAGE_ADDR + 4));
//   t = *((uint32_t *)(PAGE_ADDR + 8));
//   x = x;

//   while (1) {
//   }
// }
