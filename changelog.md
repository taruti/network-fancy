# network-fancy changelog

## Version 0.2.1 2014-12-09

- Fix strerror_r symbol mangling with glibc

## Version 0.2.0 2014-12-09

- Use NetworkExceptions for precise error reporting
- Handle unix domain sockets wich don't set SocketAddr on recvFrom
