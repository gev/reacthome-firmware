#include "onewire.h"

#define TIME_RESET_US 480
#define TIME_PRESENCE_FIRST_US 70
#define TIME_PRESENCE_SECOND_US 410
#define TIME_SEND_BIT_1_US 6
#define TIME_SEND_BIT_1_PAUSE_US 64
#define TIME_SEND_BIT_0_US 60
#define TIME_SEND_BIT_0_PAUSE_US 10
#define TIME_READ_BIT_US 9
#define TIME_READ_BIT_PAUSE_US 55

#define OW_PORT     GPIOA
#define OW_PIN      GPIO_PIN_8

typedef enum {
    p_reset = 0,
    p_write_bit,
    p_read_bit,
    p_write_buf,
    p_read_buf,
} protocol_states_t;

typedef enum {
    r_start = 0,
    r_delay_reset,
    r_delay_presence,
    r_wait_end,
    r_done,
} reset_states_t;

// typedef enum {

// } write_byte_state_t;

// typedef enum {
//     w_bit_start = 0,

// } write_bit_state_t;

protocol_states_t protocol_states;
reset_states_t reset_states;
bool ow_presence_flag = false; 


void init_onewire(){
  rcu_periph_clock_enable(RCU_GPIOA);
  gpio_mode_set(OW_PORT, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, OW_PIN);
  gpio_output_options_set(OW_PORT, GPIO_OTYPE_OD, GPIO_OSPEED_MAX, OW_PIN);
  gpio_bit_set(OW_PORT, OW_PIN);

  rcu_periph_clock_enable(RCU_TIMER2);

    nvic_irq_enable(TIMER2_IRQn, 0, 0);

  timer_parameter_struct timer_initpara;

    timer_deinit(TIMER2);

    timer_struct_para_init(&timer_initpara);

    timer_initpara.prescaler         = 83;
    timer_initpara.alignedmode       = TIMER_COUNTER_EDGE;
    timer_initpara.counterdirection  = TIMER_COUNTER_UP;
    timer_initpara.period            = 0xFF;
    timer_initpara.clockdivision     = TIMER_CKDIV_DIV1;
    timer_init(TIMER2, &timer_initpara);

    /* clear channel 0 interrupt bit */
    timer_interrupt_flag_clear(TIMER2, TIMER_INT_FLAG_UP);
    /* enable the TIMER interrupt */
    timer_interrupt_flag_clear(TIMER2, TIMER_INT_UP);
    /* enable a TIMER */
    timer_enable(TIMER2);

}

void ow_reset(){
    protocol_states = p_reset;
    reset_states = r_start;
    ow_handler();
}

void ow_write_buf(uint8_t *value, uint16_t size){

}

uint8_t ow_read_buf(uint8_t *buf, uint8_t size){
    static extern uint8_t size_buf = size;
    protocol_states = p_write_buf;
    reset_states = r_start;
}

void ow_write_bit(uint8_t value){
    
}

uint8_t ow_read_bit(){
    
}

void irq_timer_delay_us(uint16_t time){
    timer_counter_value_config (TIMER2, 0xFF - (time-1));
    timer_interrupt_flag_clear(TIMER2, TIMER_INT_UP);
    timer_interrupt_enable(TIMER2, TIMER_INT_UP);
}

inline void reset_handler(){
    switch (reset_states){
        case r_start:
            gpio_bit_reset(OW_PORT, OW_PIN);
            reset_states = r_delay_reset;
            irq_timer_delay_us(TIME_RESET_US);
            break;
        case r_delay_reset:
            gpio_bit_set(OW_PORT, OW_PIN);
            reset_states = r_delay_presence;
            irq_timer_delay_us(TIME_PRESENCE_FIRST_US);
            break;
        case r_delay_presence:
            if(gpio_input_bit_get(OW_PORT, OW_PIN)){
                ow_presence_flag = false;
            } else {
                ow_presence_flag = true;
            }
            reset_states = r_wait_end;
            irq_timer_delay_us(TIME_PRESENCE_FIRST_US);
            break;
        case r_wait_end:
            reset_states = r_done;
            irq_timer_delay_us(TIME_PRESENCE_SECOND_US);
            break;
        case r_done:
            break;
        }
}

inline void write_buf_handler(){
    static uint8_t i = 0;
    
}

void ow_handler(){
    switch (protocol_states){
    case p_reset:
        reset_handler()
    case p_write_buf:
        write_buf_handler()
    default :
        break;

    }

}

void TIMER2_IRQHandler(void)
{
    if(SET == timer_interrupt_flag_get(TIMER2, TIMER_INT_UP)) {
        timer_interrupt_flag_clear(TIMER2, TIMER_INT_UP);
        timer_interrupt_disable(TIMER2, TIMER_INT_UP);
        ow_handler();
    }
    
}