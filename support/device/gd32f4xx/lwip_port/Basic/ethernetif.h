#ifndef __ETHERNETIF_H__
#define __ETHERNETIF_H__


#include "lwip/err.h"
#include "lwip/netif.h"

#define USE_ENET_INTERRUPT

#define RMII_MODE 

/* MAC address: MAC_ADDR0:MAC_ADDR1:MAC_ADDR2:MAC_ADDR3:MAC_ADDR4:MAC_ADDR5 */
#define MAC_ADDR0   4
#define MAC_ADDR1   4
#define MAC_ADDR2   4
#define MAC_ADDR3   4
#define MAC_ADDR4   4
#define MAC_ADDR5   4
 
/* network interface name */
#define IFNAME0 'G'
#define IFNAME1 'D'

err_t ethernetif_init(struct netif *netif);
err_t ethernetif_input(struct netif *netif);

#endif
