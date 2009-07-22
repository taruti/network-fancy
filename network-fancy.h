#ifndef __NETWORK_FANCY_H__
#define __NETWORK_FANCY_H__

#ifndef WINDOWS
#include <arpa/inet.h>
#include <errno.h>
#include <netdb.h>
#include <sys/socket.h>
#include <sys/un.h>
#define SAFE_ON_WIN unsafe

#else /* WINDOWS */

#include <winsock2.h>
#include <ws2tcpip.h>
#define sa_family_t short
#define AI_NUMERICSERV 0

struct network_fancy_aaccept {
  SOCKET s;
  struct sockaddr *addr;
  int alen;
};
#define SAFE_ON_WIN safe

#endif


#endif
