# network-fancy changelog

## Master - not yet released to hackage

- Fixes for ancient GHC, 32 bit platform woes, fix #8
- Fixes for ancient GHC, make imports stricter, fix #7

## Version 0.2.3 2015-06-20

- Fix OS X build

## Version 0.2.2 2014-12-13

- Modify Setup.hs to keep nix happy, closes #4
- Depend on pthread always on linux
- Show more informative error strings
- Wibble imports
- ensure estat is zero if getsockopt is broken

## Version 0.2.1 2014-12-09

- Fix strerror_r symbol mangling with glibc

## Version 0.2.0 2014-12-09

- Use NetworkExceptions for precise error reporting
- Handle unix domain sockets wich don't set SocketAddr on recvFrom
