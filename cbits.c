#include <errno.h>
#include <sys/socket.h>

int getSocketError(void) { return errno; }

int getsockopt_error(int fd) {
  int estat, res;
  socklen_t len = sizeof(int);
  res = getsockopt(fd, SOL_SOCKET, SO_ERROR, &estat, &len);
  return (res == 0) ? estat : errno;
}

/*
#if IMPL == EPOLL

#include <sys/epoll.h>

int EPollCtl(int efd, int fd, void *sptr) {
  struct epoll_event ev;
  ev.events = EPOLLERR | EPOLLHUP | EPOLLOUT | EPOLLIN | EPOLLET;
  ev.data.ptr = sptr;
  return epoll_ctl(efd, EPOLL_CTL_ADD, fd, &ev);
}

int EPollGetEvents(struct epoll_event *ev, int n) {
  return ev[n].events;
}

void* EPollGetPtr(struct epoll_event *ev, int n) {
  return ev[n].data.ptr;
}


#endif
*/
