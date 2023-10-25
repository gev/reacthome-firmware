#include "igmp_test.h"
#include "lwip/udp.h"
#include "lwip/igmp.h"
#include "lwip/prot/igmp.h"
#include  <stdbool.h>


void igmp_echoserver_receive_callback(void *arg, struct udp_pcb *upcb, struct pbuf *p, const ip_addr_t *addr, u16_t port)
{
  printf("udp_callback\n");

//   /* Connect to the remote client */
//   udp_connect(upcb, addr, port);

//   /* Tell the client that we have accepted it */
//   udp_send(upcb, p);

//   /* free the UDP connection, so we can accept new clients */
//   udp_disconnect(upcb);
	
  /* Free the p buffer */
  pbuf_free(p);
   
}

void igmp_test_init(){

// igmp_input(struct pbuf *p, struct netif *inp, const ip4_addr_t *dest)
  struct udp_pcb *upcb;
  upcb = udp_new();
  err_t err;
  ip_addr_t igmp_group;
  IP4_ADDR(&igmp_group, 235, 1, 1, 1);
  if (upcb)
   {
     /* Bind the upcb to the UDP_PORT port */
     /* Using IP_ADDR_ANY allow the upcb to be used by any local interface */
      err = udp_bind(upcb, IP_ADDR_ANY, 23);
      
      if(err == ERR_OK)
      {
        /* Set a receive callback for the upcb */
        udp_recv(upcb, igmp_echoserver_receive_callback, NULL);
      }
   }

}
