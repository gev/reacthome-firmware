/*!
    \file    netconf.c
    \brief   network connection configuration

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

#include "lwip/mem.h"
#include "lwip/memp.h"
#include "lwip/tcp.h"
#include "lwip/udp.h"
#include "netif/etharp.h"
#include "lwip/dhcp.h"
#include "ethernetif.h"
#include "stdint.h"
#include "main.h"
#include "netconf.h"
#include <stdio.h>
#include "lwip/priv/tcp_priv.h"
#include "lwip/timeouts.h"
#include "lwip/igmp.h"

#define MAX_DHCP_TRIES        4

typedef enum {
    DHCP_START = 0,
    DHCP_WAIT_ADDRESS,
    DHCP_ADDRESS_ASSIGNED,
    DHCP_TIMEOUT
} dhcp_state_enum;

#ifdef USE_DHCP
uint32_t dhcp_fine_timer = 0;
uint32_t dhcp_coarse_timer = 0;
dhcp_state_enum dhcp_state = DHCP_START;
#endif /* USE_DHCP */

struct netif g_mynetif;
uint32_t tcp_timer = 0;
uint32_t arp_timer = 0;
uint32_t igmp_timer = 0;
ip_addr_t ip_address = {0};

void lwip_dhcp_process_handle(void);
void lwip_netif_status_callback(struct netif *netif);


err_t igmp_mac_filter (struct netif *netif,
       const ip4_addr_t *group, enum netif_mac_filter_action action)
       {
           return ERR_OK;
       };

/*!
    \brief      initializes the LwIP stack
    \param[in]  none
    \param[out] none
    \retval     none
*/
void lwip_stack_init(void)
{
    ip_addr_t ipaddr;
    ip_addr_t netmask;
    ip_addr_t gw;
    ip_addr_t igmp_group;

    /* initializes the dynamic memory heap defined by MEM_SIZE */
    mem_init();

    /* initializes the memory pools defined by MEMP_NUM_x */
    memp_init();



    IP4_ADDR(&ipaddr, IP_ADDR0, IP_ADDR1, IP_ADDR2, IP_ADDR3);
    IP4_ADDR(&netmask, NETMASK_ADDR0, NETMASK_ADDR1, NETMASK_ADDR2, NETMASK_ADDR3);
    IP4_ADDR(&gw, GW_ADDR0, GW_ADDR1, GW_ADDR2, GW_ADDR3);
    IP4_ADDR(&igmp_group, 235, 1, 1, 1);


    /* - netif_add(struct netif *netif, ip_addr_t *ipaddr,
              ip_addr_t *netmask, ip_addr_t *gw,
              void *state, err_t (* init)(struct netif *netif),
              err_t (* input)(struct pbuf *p, struct netif *netif))

     Adds your network interface to the netif_list. Allocate a struct
    netif and pass a pointer to this structure as the first argument.
    Give pointers to cleared ip_addr structures when using DHCP,
    or fill them with sane numbers otherwise. The state pointer may be NULL.

    The init function pointer must point to a initialization function for
    your ethernet netif interface. The following code illustrates it's use.*/

    netif_add(&g_mynetif, &ipaddr, &netmask, &gw, NULL, &ethernetif_init, &ethernet_input);
    // g_mynetif.flags |= NETIF_FLAG_IGMP;
    /* registers the default network interface */
    // netif_set_igmp_mac_filter(&g_mynetif, igmp_mac_filter);
    netif_set_default(&g_mynetif);
    netif_set_status_callback(&g_mynetif, lwip_netif_status_callback);

    /* when the netif is fully configured this function must be called */
    igmp_init();
    int error = igmp_start(&g_mynetif);
    printf("err: %i \n", error);
    igmp_joingroup_netif(&g_mynetif, &igmp_group);
    netif_set_up(&g_mynetif);

}

/*!
    \brief      called when a frame is received
    \param[in]  none
    \param[out] none
    \retval     none
*/
void lwip_pkt_handle(void)
{
    /* read a received packet from the Ethernet buffers and send it to the lwIP for handling */
    ethernetif_input(&g_mynetif);
}

/*!
    \brief      LwIP periodic tasks
    \param[in]  localtime the current LocalTime value
    \param[out] none
    \retval     none
*/
void lwip_periodic_handle(__IO uint32_t localtime)
{
// #if LWIP_TCP
//     /* TCP periodic process every 250 ms */
//     if(localtime - tcp_timer >= TCP_TMR_INTERVAL) {
//         tcp_timer =  localtime;
//         tcp_tmr();
//     }

// #endif /* LWIP_TCP */

    /* ARP periodic process every 5s */
    if((localtime - arp_timer) >= ARP_TMR_INTERVAL) {
        arp_timer = localtime;
        etharp_tmr();
    }

    /* ARP periodic process every 5s */
    if((localtime - igmp_timer) >= IGMP_TMR_INTERVAL) {
        igmp_timer = localtime;
        igmp_tmr();
    }


}


unsigned long sys_now(void)
{
    extern volatile unsigned int g_localtime;
    return g_localtime;
}
