#ifndef GD32F450I_EVAL_H
#define GD32F450I_EVAL_H

#include "gd32f4xx.h"

#define COMn                             1U
#define EVAL_COM0                        USART0
#define EVAL_COM0_CLK                    RCU_USART0

#define EVAL_COM0_TX_PIN                 GPIO_PIN_9
#define EVAL_COM0_RX_PIN                 GPIO_PIN_10

#define EVAL_COM0_GPIO_PORT              GPIOA
#define EVAL_COM0_GPIO_CLK               RCU_GPIOA
#define EVAL_COM0_AF                     GPIO_AF_7


/* configure COM port */
void gd_eval_com_init(uint32_t com);

 #endif