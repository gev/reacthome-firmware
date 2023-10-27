#include "udp_echo.h"
#include "lwip/udp.h"
#include <string.h>
#include <stdio.h>
#include "gd32f4xx.h"

void udp_echoserver_receive_callback(void *arg, struct udp_pcb *upcb, struct pbuf *p, const ip_addr_t *addr, u16_t port)
{
  printf("udp_callback\n");

  /* Connect to the remote client */
  udp_connect(upcb, addr, port);

  /* Tell the client that we have accepted it */
  udp_send(upcb, p);

  /* free the UDP connection, so we can accept new clients */
  udp_disconnect(upcb);
	
  /* Free the p buffer */
  pbuf_free(p);
   
}

void udp_echo_init(void){
  struct udp_pcb *upcb;
  upcb = udp_new();
  err_t err;
  if (upcb)
   {
     /* Bind the upcb to the UDP_PORT port */
     /* Using IP_ADDR_ANY allow the upcb to be used by any local interface */
      err = udp_bind(upcb, IP_ADDR_ANY, 2000);
      
      if(err == ERR_OK)
      {
        /* Set a receive callback for the upcb */
        udp_recv(upcb, udp_echoserver_receive_callback, NULL);
      }
   }

}
