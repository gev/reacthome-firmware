#include "igmp_test.h"
#include "lwip/udp.h"
#include "lwip/igmp.h"
#include "lwip/prot/igmp.h"
#include  <stdbool.h>


void igmp_echoserver_receive_callback(void *arg, struct udp_pcb *upcb, struct pbuf *p, const ip_addr_t *addr, u16_t port)
{
  printf("udp_callback\n");

  udp_connect(upcb, addr, port);

  udp_send(upcb, p);

  udp_disconnect(upcb);
	
  pbuf_free(p);
   
}

void igmp_test_init(){

  struct udp_pcb *upcb;
  upcb = udp_new();
  err_t err;
  if (upcb)
   {
      err = udp_bind(upcb, IP_ADDR_ANY, 2000);

      if(err == ERR_OK)
      {
        udp_recv(upcb, igmp_echoserver_receive_callback, NULL);
      }
   }

}
