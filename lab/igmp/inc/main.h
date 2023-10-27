#ifndef MAIN_H
#define MAIN_H

#include "gd32f4xx.h"
#include "stdint.h"
#include "gd32f4xx_enet_eval.h"


#define USE_ENET_INTERRUPT

/* MAC address: MAC_ADDR0:MAC_ADDR1:MAC_ADDR2:MAC_ADDR3:MAC_ADDR4:MAC_ADDR5 */
#define MAC_ADDR0   4
#define MAC_ADDR1   4
#define MAC_ADDR2   4
#define MAC_ADDR3   4
#define MAC_ADDR4   4
#define MAC_ADDR5   4
 
/* static IP address: IP_ADDR0.IP_ADDR1.IP_ADDR2.IP_ADDR3 */
#define IP_ADDR0   192
#define IP_ADDR1   168
#define IP_ADDR2   88
#define IP_ADDR3   9

/* remote IP address: IP_S_ADDR0.IP_S_ADDR1.IP_S_ADDR2.IP_S_ADDR3 */
// #define IP_S_ADDR0   192
// #define IP_S_ADDR1   168
// #define IP_S_ADDR2   88
// #define IP_S_ADDR3   154


   
/* net mask */
#define NETMASK_ADDR0   255
#define NETMASK_ADDR1   255
#define NETMASK_ADDR2   255
#define NETMASK_ADDR3   0

/* gateway address */
#define GW_ADDR0   192
#define GW_ADDR1   168
#define GW_ADDR2   88
#define GW_ADDR3   1


#define RMII_MODE 

/* function declarations */
/* updates the system local time */
void time_update(void);
/* insert a delay time */
void delay_10ms(uint32_t ncount);

#endif /* MAIN_H */
