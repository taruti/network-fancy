#include "network-fancy.h"

int getSocketError(void) { return errno; }

int getsockopt_error(int fd) {
  int estat, res;
  socklen_t len = sizeof(int);
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
