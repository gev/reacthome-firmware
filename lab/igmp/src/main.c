/*!
    \file    main.c
    \brief   enet demo

    \version 2016-08-15, V1.0.0, firmware for GD32F4xx
    \version 2018-12-12, V2.0.0, firmware for GD32F4xx
    \version 2020-09-30, V2.1.0, firmware for GD32F4xx
    \version 2022-03-09, V3.0.0, firmware for GD32F4xx
*/

/*
    Copyright (c) 2022, GigaDevice Semiconductor Inc.

    Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this
       list of conditions and the following disclaimer.
    2. Redistributions in binary form must reproduce the above copyright notice,
       this list of conditions and the following disclaimer in the documentation
       and/or other materials provided with the distribution.
    3. Neither the name of the copyright holder nor the names of its contributors
       may be used to endorse or promote products derived from this software without
       specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
OF SUCH DAMAGE.
*/

#include "gd32f4xx.h"
#include "netconf.h"
#include "main.h"
#include "lwip/timeouts.h"
#include "gd32f450i_eval.h"
#include <stdio.h>
#include "lwip/igmp.h"
#include "lwip/prot/igmp.h"
#include "igmp_test.h"



#define SYSTEMTICK_PERIOD_MS  10

__IO uint32_t g_localtime = 0; /* for creating a time reference incremented by 10ms */
uint32_t g_timedelay;

int _write(int file, char *ptr, int len);

void uart_stdout_init(void)
{
    setvbuf(stdout, NULL, _IONBF, 0);
}

int main(void)
{
    gd_eval_com_init(EVAL_COM0);
    uart_stdout_init();
    enet_system_setup();
    lwip_stack_init();

    while(1) {

        lwip_periodic_handle(g_localtime);

    }
}

void lwip_netif_status_callback(struct netif *netif)
{
    if((netif->flags & NETIF_FLAG_UP) != 0) {
        igmp_test_init();
    }
}

void time_update(void)
{
    g_localtime += SYSTEMTICK_PERIOD_MS;
}




int _write(int file, char *ptr, int len)
{
    int i;
    for (i = 0; i < len; i++)
    {
        /* отправить символ один за другим */
        while (!usart_flag_get(USART0, USART_FLAG_TBE));
        usart_data_transmit(USART0, ptr[i]);
    }
    return len;
}
