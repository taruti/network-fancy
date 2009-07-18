#ifndef WINDOWS
#include <arpa/inet.h>
#include <errno.h>
#include <netdb.h>
#include <sys/socket.h>
#include <sys/un.h>
#else
#include <winsock2.h>
#include <ws2tcpip.h>
#define sa_family_t short
#define AI_NUMERICSERV 0
#endif

int getSocketError(void) { return errno; }

int getsockopt_error(int fd) {
  int estat, res;
  socklen_t len = sizeof(int);
  res = getsockopt(fd, SOL_SOCKET, SO_ERROR, &estat, &len);
  return (res == 0) ? estat : errno;
}
