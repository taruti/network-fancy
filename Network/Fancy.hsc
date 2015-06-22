module Network.Fancy
    (-- * Address
     HostName, Address(..),
     -- * Stream clients
     withStream, connectStream,
     -- * Datagram clients
     connectDgram, withDgram, StringLike, recv,send, closeSocket,
     -- * Servers
     ServerSpec(..), serverSpec,
     Threading(..), Reverse(..),
     streamServer, dgramServer, sleepForever,
     -- * Other
     getCurrentHost,
     Socket,
     NetworkException(..),
    ) where

import Control.Concurrent
import Control.Exception as E(bracket, finally, try, SomeException)
import Control.Monad(when, forM)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy  as L
import Data.List(intercalate)
import Data.Typeable(Typeable)
#if __GLASGOW_HASKELL__ >= 708
import Foreign
#else
import Foreign hiding (unsafeForeignPtrToPtr)
#endif
import Foreign.C(CString,peekCString,withCString,Errno(..),eAGAIN,eINTR,eWOULDBLOCK,getErrno,eINPROGRESS)
import Foreign.C.Types
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Numeric(showHex)
import System.IO(Handle, hClose, IOMode(ReadWriteMode))
import System.IO.Unsafe
import System.Posix.Internals hiding(c_close)
#if __GLASGOW_HASKELL__ > 610
import GHC.IO.Device
import GHC.IO.Handle.FD(fdToHandle')
#else
import GHC.Handle(fdToHandle')
#endif
#ifdef WINDOWS
import GHC.Conc(asyncDoProc)
#endif

import Network.Fancy.Error
import Network.Fancy.Internal

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

#ifndef  AI_NUMERICSERV
#define AI_NUMERICSERV 0
#endif

#ifdef FREEBSD
#include <netinet6/in6.h>
#endif /* FREEBSD */

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

#endif /* WINDOWS */

setNonBlockingFD' :: FD -> IO ()
setNonBlockingFD' fd =
#if __GLASGOW_HASKELL__ < 611
    System.Posix.Internals.setNonBlockingFD fd
#else
    System.Posix.Internals.setNonBlockingFD fd True
#endif

type HostName = String

data Address = IP   HostName Int -- ^ Host name and port, either IPv4 or IPv6.
             | IPv4 HostName Int -- ^ Host name and port, only IPv4.
             | IPv6 HostName Int -- ^ Host name and port, only IPv6
             | Unix FilePath     -- ^ Local unix socket, not supported on Windows.
               deriving(Eq,Ord,Show,Typeable)

class StringLike string where
    toBS   :: string -> B.ByteString
    fromBS :: B.ByteString -> string

instance StringLike String where
    toBS   = B.pack
    fromBS = B.unpack

instance StringLike L.ByteString where
    toBS   = B.concat . L.toChunks
    fromBS = \x -> L.fromChunks [x]

instance StringLike B.ByteString where
    toBS   = id
    fromBS = id

-- | Send the string as one chunk
send :: StringLike string => Socket -> string -> IO ()
send (Socket s) bs =
  let loop ptr len = do
        r <- writeOp "send" s (c_send s (castPtr ptr) (fromIntegral len) 0)
        let r' = fromIntegral r
        if r' >= len then return () else loop (plusPtr ptr r') (r' - len)
  in B.unsafeUseAsCStringLen (toBS bs) $ \(ptr,len) -> loop ptr len
-- | Receive one chunk with given maximum size
recv :: StringLike string => Socket -> Int -> IO string
recv (Socket s) len= fmap fromBS (
                     B.createAndTrim len $ \ptr -> do
                     r <- readOp "recv" s (c_recv s (castPtr ptr) (fromIntegral len) 0)
                     return $ fromIntegral r)

recvFrom :: StringLike string => Socket -> Int -> SocketAddress -> IO (string,SocketAddress)
recvFrom (Socket s) buflen (SA _ salen) = do
  sa <- mallocForeignPtrBytes salen
  withForeignPtr sa $ \sa_ptr -> do
  (str,len) <- B.createAndTrim' buflen $ \ptr -> do
    with (fromIntegral salen) $ \salen_ptr -> do
    rd  <- readOp "recvfrom" s $ c_recvfrom s ptr (fromIntegral buflen) 0 sa_ptr salen_ptr
    len <- peek salen_ptr
    return (0,fromIntegral rd, fromIntegral len)
  return (fromBS str, SA sa len)

sendTo :: StringLike string => SocketAddress -> Socket -> string -> IO ()
sendTo (SA sa salen) (Socket s) str = do
  withForeignPtr sa $ \sa_ptr -> do
  let loop ptr len = do
        r <- writeOp "sendTo" s $ c_sendto s (castPtr ptr) (fromIntegral len) 0 sa_ptr (fromIntegral salen)
        let r' = fromIntegral r
        if r' >= len then return () else loop (plusPtr ptr r') (r' - len)
  B.unsafeUseAsCStringLen (toBS str) $ \(ptr,len) -> loop ptr len

foreign import CALLCONV SAFE_ON_WIN "recv" c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO (#type ssize_t)
foreign import CALLCONV SAFE_ON_WIN "send" c_send :: CInt -> Ptr Word8 -> CSize -> CInt -> IO (#type ssize_t)
foreign import CALLCONV SAFE_ON_WIN "recvfrom" c_recvfrom :: CInt -> Ptr Word8 -> CSize -> CInt -> Ptr () -> Ptr SLen -> IO (#type ssize_t)
foreign import CALLCONV SAFE_ON_WIN "sendto" c_sendto :: CInt -> Ptr Word8 -> CSize -> CInt -> Ptr () -> SLen -> IO (#type ssize_t)

-- | Close the socket specified.
closeSocket :: Socket -> IO ()
closeSocket sock@(Socket fd) = throwIfError_ sock "close" $ c_close fd

foreign import CALLCONV unsafe "bind"    c_bind    :: CInt -> Ptr () -> (SLen) -> IO CInt
foreign import CALLCONV unsafe "listen"  c_listen  :: CInt -> CInt -> IO CInt
foreign import CALLCONV unsafe "socket"  c_socket  :: CFamily -> CType -> CInt -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "connect" c_connect :: CInt -> Ptr () -> (SLen) -> IO CInt
#ifdef WINDOWS
foreign import CALLCONV unsafe "closesocket" c_close :: CInt -> IO CInt
#else
foreign import CALLCONV unsafe "close" c_close :: CInt -> IO CInt
#endif


-- | Open a datagram (udp) socket for the given block and close it afterwards.
withDgram :: Address -> (Socket -> IO a) -> IO a
withDgram a = bracket (connectDgram a) closeSocket

-- | Open a stream (tcp) socket for the given block and close it afterwards.
withStream :: Address -> (Handle -> IO a) -> IO a
withStream a = bracket (connectStream a) hClose

-- | Open a stream (tcp) socket.
connectStream :: Address -> IO Handle
connectStream addr = a2sas sockStream aiNumericserv addr >>= csas (connect sockStream) >>= socketToHandle

-- | Open a datagram (udp) socket.
connectDgram  :: Address -> IO Socket
connectDgram addr = a2sas sockDgram aiNumericserv addr >>= csas (connect sockDgram)

socketToHandle :: Socket -> IO Handle
#if __GLASGOW_HASKELL__ <= 610
socketToHandle (Socket fd) = fdToHandle' (fromIntegral fd) (Just Stream) True (show fd) ReadWriteMode True
#else
socketToHandle (Socket fd) = fdToHandle' (fromIntegral fd) (Just GHC.IO.Device.Stream) True (show fd) ReadWriteMode True
#endif


connect :: CType -> SocketAddress -> IO Socket
connect stype (SA sa len) = do
  fam <- getFamily (SA sa len)
  s   <- newsock fam stype
  setNonBlockingFD' s
  let sock = Socket s
  let loop = do r   <- withForeignPtr sa $ \ptr -> c_connect s ptr (fromIntegral len)
                err <- getErrno
                case r of
                  -1 | err == eINTR       -> do loop
                     | err == eINPROGRESS -> do threadWaitWrite (fromIntegral s)
                                                soe <- getsockopt_error s
                                                if soe==0 then return sock else throwNetworkException sock "connect" (Errno soe)
                     |  otherwise         -> do throwNetworkException sock "connect" err
                  _                       -> do return sock
  loop

foreign import ccall unsafe getsockopt_error :: CInt -> IO CInt


-- | Get the family \(domain\) of the socket.
getFamily :: SocketAddress -> IO CFamily
getFamily (SA sa _) = worker >>= return . fromIntegral
    where worker :: IO #type sa_family_t
          worker = withForeignPtr sa (#peek struct sockaddr, sa_family)

csas :: (SocketAddress -> IO a) -> [SocketAddress] -> IO a
csas _ []       = throwOther NoSuchHostException
csas c [sa]     = c sa
csas c (sa:sas) = do x <- try' (c sa)
                     case x of
                      (Left _)  -> csas c sas
                      (Right v) -> return v

try' :: IO a -> IO (Either SomeException a)
try' = E.try

ssize64 :: (#type ssize_t) -> Int64
ssize64 = fromIntegral

writeOp :: String -> CInt -> IO (#type ssize_t) -> IO Int64
writeOp desc s op = loop
    where fd = fromIntegral s
          loop = do res <- fmap ssize64 op
                    if res /= -1 then return res else getErrno >>= eh
          eh err | err == eINTR = loop
                 | err == eWOULDBLOCK || err == eAGAIN = threadWaitWrite fd >> loop
                 | True = throwNetworkException (Socket s) desc err

readOp :: String -> CInt -> IO (#type ssize_t) -> IO Int64
readOp desc s op = loop
    where fd = fromIntegral s
          loop = do res <- fmap ssize64 op
                    if res /= -1 then return res else getErrno >>= eh
          eh err | err == eINTR = loop
                 | err == eWOULDBLOCK || err == eAGAIN = threadWaitRead fd >> loop
                 | True = throwNetworkException (Socket s) desc err



withResolverLock :: IO a -> IO a
#ifdef WINDOWS
withResolverLock c = resolverLock `seq` c
{-# NOINLINE resolverLock #-}
resolverLock :: CInt
resolverLock = unsafePerformIO $ allocaBytes (#size struct WSAData) (wsaStartup 0x0002)
foreign import stdcall safe "WSAStartup" wsaStartup :: Int -> Ptr a -> IO CInt
#else
withResolverLock x = x
#endif

data SocketAddress = SA !(ForeignPtr ()) !Int

instance Show SocketAddress where show _ = "SocketAddress"

type AddrInfoT     = Word8

type CFamily      = Int
type CType        = Int


#enum CFamily, , AF_INET, AF_INET6, AF_UNSPEC
#ifndef WINDOWS
#enum CFamily, ,AF_LOCAL
#endif

#enum CType, , SOCK_STREAM, SOCK_DGRAM

a2sas :: CType -> CInt -> Address -> IO [SocketAddress]
a2sas t f (IP   hn p)        = getAddrInfo hn (show p) f afUnspec t
a2sas t f (IPv4 hn p)        = getAddrInfo hn (show p) f afInet t
a2sas t f (IPv6 hn p)        = getAddrInfo hn (show p) f afInet6 t
#ifdef WINDOWS
a2sas _ _ (Unix _)           = throwOther UnsupportedAddressFamily
#else
a2sas _ _ (Unix fp)          = do let maxSize = ((#size struct sockaddr_un)-(#offset struct sockaddr_un, sun_path))
                                  when (length fp >= maxSize) $ throwOther AddressTooLongException
                                  sa <- mallocForeignPtrBytes $ fromIntegral salLocal
                                  withForeignPtr sa $ \sa_ptr -> do
                                  (#poke struct sockaddr_un, sun_family) sa_ptr afLocal
                                  let tw :: Char -> Word8
                                      tw = toEnum . fromEnum
                                  pokeArray0 0 ((#ptr struct sockaddr_un, sun_path) sa_ptr) $ map tw fp
                                  return [SA sa salLocal]
salLocal :: Int
salLocal   =  #size struct sockaddr_un

#endif

aiPassive, aiNumericserv :: CInt
aiNumericserv = #const AI_NUMERICSERV
aiPassive = #const AI_PASSIVE


getAddrInfo :: String     -- ^ The hostname.
            -> String    -- ^ Service name.
            -> CInt          -- ^ Flags, a combination of 'aiPassive' 'aiNumerichost' and 'aiNumericserv'.
            -> CFamily -- ^ Family
            -> CType -- ^ Socket type
            -> IO [SocketAddress]


getAddrInfo host serv flags fam typ = withResolverLock $ do
  let unai :: Ptr AddrInfoT -> IO [SocketAddress]
      unai ai | ai == nullPtr = return []
              | otherwise     = uwork ai
      uwork ai = do sal'<- (#peek struct addrinfo, ai_addrlen)   ai :: IO SLen
                    sa' <- (#peek struct addrinfo, ai_addr)      ai
                    let sal = fromIntegral sal'
                    sa  <- mallocForeignPtrBytes sal
                    copyBytes (unsafeForeignPtrToPtr sa) sa' sal
                    next<- (#peek struct addrinfo, ai_next)      ai
                    rest<- unai next
                    return ((SA sa sal):rest)
      getAI :: IO (Ptr AddrInfoT)
      getAI = allocaBytes (#size struct addrinfo) $ \hints -> do
              _ <- B.memset hints 0 (#size struct addrinfo)
              (#poke struct addrinfo, ai_flags)    hints flags
              (#poke struct addrinfo, ai_family)   hints fam
              (#poke struct addrinfo, ai_socktype) hints typ
              withStr host $ \host_buf -> do
              withStr serv $ \serv_buf -> do
              with nullPtr $ \result   -> do
              throwGAIErrorIf $ c_getaddrinfo host_buf serv_buf hints result
              peek result
      withStr :: String -> (CString -> IO a) -> IO a
      withStr "" fun = fun nullPtr
      withStr s  fun = withCString s fun
  bracket getAI c_freeaddrinfo unai

foreign import CALLCONV unsafe "freeaddrinfo" c_freeaddrinfo :: Ptr AddrInfoT -> IO ()
foreign import CALLCONV   safe "getaddrinfo"  c_getaddrinfo  :: Ptr CChar -> Ptr CChar ->
                                                                Ptr AddrInfoT -> Ptr (Ptr AddrInfoT) ->
                                                                IO CInt




--hostNameToNumber :: Address -> IO [Address]
--hostNameToNumber = error "FIXME"
--hostNumberToName :: Address -> IO [Address]
--hostNumberToName = error "FIXME"

-- | Get the current hostname.
getCurrentHost :: IO HostName
getCurrentHost = do
  allocaArray 256 $ \buffer -> do
    throwIfError_ invSock "gethostname" $ c_gethostname buffer 256
    peekCString buffer

foreign import CALLCONV unsafe "gethostname" c_gethostname :: Ptr CChar -> CSize -> IO CInt

invSock :: Socket
invSock = Socket (-1)

-- SERVERS

data Threading  = Threaded  -- ^ Run each request in a separate thread without blocking the server loop.
                | Inline    -- ^ Run each request inline inside the request loop.
data Reverse    = ReverseNumeric -- ^ Use numeric addresses for peers.
                | ReverseName    -- ^ Resolve reverse names if possible for peers.

data ServerSpec = ServerSpec
    { address         :: Address    -- ^ Address for the server. Use hostname \"\" to bind to all interfaces.
    , reverseAddress  :: Reverse    -- ^ Should the address of connecting clients be suplied numerically or as a name to server function.
    , threading       :: Threading  -- ^ Handle requests Inline or Threaded.
    , closeConnection :: Bool       -- ^ Close the client connection automatically after the ServerFun finishes.
    , recvSize        :: Int        -- ^ Buffer size for receiving datagrams.
    }

-- | Default server specification
serverSpec :: ServerSpec
serverSpec = ServerSpec { address   = IP "" 0
                        , reverseAddress   = ReverseNumeric
                        , threading = Threaded
                        , closeConnection = True
                        , recvSize        = 4096
                        }

-- | Run a stream (tcp) server. The function does not block, use sleepForever if that is desired.
streamServer :: ServerSpec -> (Handle -> Address -> IO ()) -> IO [ThreadId]
streamServer ss sfun = do
  sas <- a2sas sockStream (aiNumericserv .|. aiPassive) (address ss)
  when (null sas) $ throwOther NoSuchHostException
  let sf ha psa = case threading ss of
                    Threaded -> forkIO (clo ha $ sfun ha psa) >> return ()
                    Inline   -> clo ha $ sfun ha psa
      clo ha = case closeConnection ss of
                True  -> \x -> x `E.finally` (hClose ha)
                False -> id
  forM sas $ \sa -> do
     fam  <- getFamily sa
     sock <- newsock fam sockStream
     setNonBlockingFD' sock
     let socket = Socket sock
     let on :: CInt
         on = 1
         os = fromIntegral $ sizeOf on
     _ <- with on $ \onptr -> c_setsockopt sock (#const SOL_SOCKET) (#const SO_REUSEADDR) onptr os
     bind socket sa
     listen socket 128
     let loop = do (s,psa) <- accept socket sa
                   a <- unsafeInterleaveIO $ case reverseAddress ss of
                          ReverseNumeric -> rnumeric psa
                          ReverseName    -> rname psa
                   ha <- socketToHandle s
                   sf ha a
                   loop
     forkIO loop

newsock :: CFamily -> CType -> IO CInt
newsock fam typ = throwIfError invSock "socket" $ c_socket fam typ 0

foreign import CALLCONV unsafe "setsockopt" c_setsockopt ::
  CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt

-- | Bind a socket to an address. Be wary of AF_LOCAL + NFS blocking?
bind :: Socket -> SocketAddress -> IO ()
bind s@(Socket fd) (SA sa len) = do
  withForeignPtr sa $ \sa_ptr ->
    throwIfError_ s "bind" $ c_bind fd sa_ptr (fromIntegral len)

-- | Listen on an socket
listen :: Socket -> Int -> IO ()
listen s@(Socket fd) iv = throwIfError_ s "listen" (c_listen fd (toEnum iv))

accept :: Socket -> SocketAddress -> IO (Socket, SocketAddress)
accept (Socket lfd) (SA _ len) = do
  sa <- mallocForeignPtrBytes len
  s  <- withForeignPtr sa $ \sa_ptr -> do
#ifndef WINDOWS
          with (fromIntegral len) $ \len_ptr -> do
            readOp "accept" lfd $ fmap fromIntegral $ c_accept lfd sa_ptr len_ptr
#else
          if threaded then with (fromIntegral len) $ \len_ptr -> do
                           throwIfError (Socket lfd) "accept" (c_accept lfd sa_ptr len_ptr)
                      else allocaBytes (#size struct network_fancy_aaccept) $ \ptr -> do
                           (#poke struct network_fancy_aaccept, s)    ptr lfd
                           (#poke struct network_fancy_aaccept, addr) ptr sa_ptr
                           (#poke struct network_fancy_aaccept, alen) ptr len
                           r <- asyncDoProc c_nf_async_accept ptr
                           when (r /= 0) $ ioError (errnoToIOError "accept" (Errno (fromIntegral r)) Nothing Nothing)
                           (#peek struct network_fancy_aaccept, s) ptr

#endif
  let s' = fromIntegral s
  setNonBlockingFD' s'
  return (Socket s',SA sa len)

foreign import CALLCONV SAFE_ON_WIN "accept"  c_accept  :: CInt -> Ptr () -> Ptr (SLen) -> IO CInt
#ifdef WINDOWS
foreign import ccall unsafe "&c_nf_async_accept" c_nf_async_accept :: FunPtr (Ptr () -> IO Int)
foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool
#endif

-- | Run a datagram (udp) server. The function does not block, use sleepForever if that is desired.
dgramServer  :: StringLike packet => ServerSpec -- ^ Server specification
             -> (packet -> Address -> IO [packet]) -- ^ The server function is given a received packet and the Address of the peer. It returns a list of reply packets to send to that peer. Note that the list elements are invidual packets, not concatenated together.
                -> IO [ThreadId] -- ^ ThreadIds of the server listener processes.
dgramServer ss sfun = do
  sas <- a2sas sockDgram (aiNumericserv .|. aiPassive) (address ss)
  when (null sas) $ throwOther NoSuchHostException
  forM sas $ \sa -> do
     fam  <- getFamily sa
     sock <- newsock fam sockDgram
     setNonBlockingFD' sock
     let socket = Socket sock
     let on :: CInt
         on = 1
         os = fromIntegral $ sizeOf on
     _ <- with on $ \onptr -> c_setsockopt sock (#const SOL_SOCKET) (#const SO_REUSEADDR) onptr os
     bind socket sa
     let loop = do (str,psa) <- recvFrom socket (recvSize ss) sa
                   a <- unsafeInterleaveIO $ case reverseAddress ss of
                          ReverseNumeric -> rnumeric psa
                          ReverseName    -> rname psa
                   case threading ss of
                    Threaded -> forkIO (mapM_ (sendTo psa socket) =<< sfun str a) >> return ()
                    Inline   -> mapM_ (sendTo psa socket) =<< sfun str a
                   loop
     forkIO loop


rnumeric, rname :: SocketAddress -> IO Address
rnumeric (SA sa len) = do
  f <- getFamily (SA sa len)
  withForeignPtr sa $ \sa_ptr -> do
  let v4fmt, v6fmt :: [Word8] -> String
      v4fmt         = intercalate "." . map show
      v6fmt xs      = if head xs == 0 then ':':':':v6map (dropWhile (==0) xs) else v6map xs
      v6map         = intercalate ":" . units
      units []      = []
      units [x]     = [showHex x ""]
      units (x:y:r) = dropWhile (=='0') (showHex x $ ldigit y) : units r
      ldigit x      = case showHex x "" of
                        [z] -> ['0',z]
                        z   -> z

  case () of
    _ | f == afInet -> do n <- fmap v4fmt $ peekArray 4 $ (#ptr struct sockaddr_in, sin_addr) sa_ptr
                          p <- ntohs =<< (#peek struct sockaddr_in, sin_port) sa_ptr
                          return $ IPv4 n (fromIntegral p)
      | f == afInet6-> do n <- fmap v6fmt $ peekArray 16 $ (#ptr struct sockaddr_in, sin_addr) sa_ptr
                          p <- ntohs =<< (#peek struct sockaddr_in6, sin6_port) sa_ptr
                          return $ IPv6 n (fromIntegral p)
#ifndef WINDOWS
      | f == afLocal-> do if len == 0 then return (Unix "") else do
                          n <- peekCString $ (#ptr struct sockaddr_un, sun_path) sa_ptr
                          return $ Unix n
#endif
      | otherwise   -> do throwOther UnsupportedAddressFamilyException

foreign import CALLCONV unsafe ntohs :: Word16 -> IO Word16

rname (SA sa len) = do
  f <- getFamily (SA sa len)
  withForeignPtr sa $ \sa_ptr -> do
  let rev = do allocaArray 256 $ \hptr -> do
               throwGAIErrorIf $ getnameinfo sa_ptr (fromIntegral len) hptr 256 nullPtr 0 0
               peekCString hptr
  case () of
    _ | f == afInet  -> do n <- rev
                           p <- ntohs =<< (#peek struct sockaddr_in, sin_port) sa_ptr
                           return $ IPv4 n (fromIntegral p)
      | f == afInet6 -> do n <- rev
                           p <- ntohs =<< (#peek struct sockaddr_in6, sin6_port) sa_ptr
                           return $ IPv6 n (fromIntegral p)
#ifndef WINDOWS
      | f == afLocal -> do n <- peekCString $ (#ptr struct sockaddr_un, sun_path) sa_ptr
                           return $ Unix n
#endif
      | otherwise    -> do throwOther UnsupportedAddressFamilyException


type SLen = #type socklen_t
foreign import CALLCONV safe getnameinfo :: Ptr () -> SLen -> Ptr CChar -> SLen -> Ptr CChar -> SLen -> CInt -> IO CInt

-- | Sleep forever. Useful after a server.
sleepForever :: IO ()
sleepForever = threadDelay maxBound >> sleepForever

