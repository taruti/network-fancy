#ifndef WINDOWS
#include <arpa/inet.h>
#include <errno.h>
#include <netdb.h>
#include <sys/socket.h>
#include <sys/un.h>
#define SAFE_ON_WIN unsafe

#ifndef AF_LOCAL
#define AF_LOCAL AF_UNIX
#endif

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
//int getSocketError(void) { return errno; }

int getsockopt_error(int fd) {
  int estat, res;
  socklen_t len = sizeof(int);
  estat = 0;
#ifdef WINDOWS
  res = getsockopt(fd, SOL_SOCKET, SO_ERROR, (char*)&estat, &len);
#else
  res = getsockopt(fd, SOL_SOCKET, SO_ERROR, &estat, &len);
#endif
  return (res == 0) ? estat : errno;
}

#ifdef WINDOWS

int c_nf_async_accept(struct network_fancy_aaccept *nfa) {
  SOCKET n = accept(nfa->s, nfa->addr, &nfa->alen);
  nfa->s = n;
  return (n != INVALID_SOCKET) ? 0 : GetLastError();
}

#endif
