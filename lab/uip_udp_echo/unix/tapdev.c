
#define UIP_DRIPADDR0   192
#define UIP_DRIPADDR1   168
#define UIP_DRIPADDR2   0
#define UIP_DRIPADDR3   1

#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <sys/socket.h>

#ifdef linux
#include <sys/ioctl.h>
#include <linux/if.h>
#include <linux/if_tun.h>
#define DEVTAP "/dev/net/tun"
#else  /* linux */
#define DEVTAP "/dev/tap0"
#endif /* linux */

#include "uip.h"

static int drop = 0;
static int fd;


/*---------------------------------------------------------------------------*/
void
tapdev_init(void)
{
  char buf[1024];
  
  fd = open(DEVTAP, O_RDWR);
  if(fd == -1) {
    perror("tapdev: tapdev_init: open");
    exit(1);
  }

#ifdef linux
  {
    struct ifreq ifr;
    memset(&ifr, 0, sizeof(ifr));
    ifr.ifr_flags = IFF_TAP|IFF_NO_PI;
    if (ioctl(fd, TUNSETIFF, (void *) &ifr) < 0) {
      perror(buf);
      exit(1);
    }
  }
#endif /* Linux */

  snprintf(buf, sizeof(buf), "ifconfig tap0 inet %d.%d.%d.%d",
	   UIP_DRIPADDR0, UIP_DRIPADDR1, UIP_DRIPADDR2, UIP_DRIPADDR3);
  system(buf);

}
/*---------------------------------------------------------------------------*/
unsigned int
tapdev_read(void)
{
  fd_set fdset;
  struct timeval tv, now;
  int ret;
  
  tv.tv_sec = 0;
  tv.tv_usec = 1000;


  FD_ZERO(&fdset);
  FD_SET(fd, &fdset);

  ret = select(fd + 1, &fdset, NULL, NULL, &tv);
  if(ret == 0) {
    return 0;
  }
  ret = read(fd, uip_buf, UIP_BUFSIZE);
  if(ret == -1) {
    perror("tap_dev: tapdev_read: read");
  }

  /*  printf("--- tap_dev: tapdev_read: read %d bytes\n", ret);*/
  /*  {
    int i;
    for(i = 0; i < 20; i++) {
      printf("%x ", uip_buf[i]);
    }
    printf("\n");
    }*/
  /*  check_checksum(uip_buf, ret);*/
  return ret;
}
/*---------------------------------------------------------------------------*/
void
tapdev_send(void)
{
  int ret;
  /*  printf("tapdev_send: sending %d bytes\n", size);*/
  /*  check_checksum(uip_buf, size);*/

  /*  drop++;
  if(drop % 8 == 7) {
    printf("Dropped a packet!\n");
    return;
    }*/
  ret = write(fd, uip_buf, uip_len);
  if(ret == -1) {
    perror("tap_dev: tapdev_send: writev");
    exit(1);
  }
}
/*---------------------------------------------------------------------------*/
