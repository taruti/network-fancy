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
     Socket
    ) where

import Control.Concurrent
import Control.Exception as E
import Control.Monad(when, forM)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy  as L
import Data.Typeable
import Foreign
import Foreign.C
import System.IO
import System.IO.Unsafe(unsafeInterleaveIO)
import GHC.Handle
#if __GLASGOW_HASKELL__ <= 610
import System.Posix.Internals hiding(c_close)
#else
import GHC.IO.Device
#endif

#ifndef WINDOWS
#include <errno.h>
#include <sys/socket.h>
#include <netdb.h>
#include <sys/un.h>
#else
#include <winsock2.h>
#include <ws2tcpip.h>
#define sa_family_t short
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
send (Socket s) bs = B.unsafeUseAsCStringLen (toBS bs) $ \(ptr,len) -> do
                     r <- throwErrnoIfMinus1RetryMayBlock "send" (c_send s (castPtr ptr) (fromIntegral len) 0) (threadWaitWrite (fromIntegral s))
                     when (r/=fromIntegral len) $ fail "send: partial packet sent!"
-- | Receive one chunk with given maximum size
recv :: StringLike string => Socket -> Int -> IO string
recv (Socket s) len= fmap fromBS (
                     B.createAndTrim len $ \ptr -> do
                     r <- throwErrnoIfMinus1RetryMayBlock "recv" (c_recv s (castPtr ptr) (fromIntegral len) 0) (threadWaitRead (fromIntegral s))
                     return $ fromIntegral r)

recvFrom :: StringLike string => Socket -> Int -> SocketAddress -> IO (string,SocketAddress)
recvFrom (Socket s) buflen (SA _ salen) = do
  sa <- mallocForeignPtrBytes salen
  withForeignPtr sa $ \sa_ptr -> do
  str<- B.createAndTrim buflen $ \ptr -> do
    with (fromIntegral salen) $ \salen_ptr -> do
    fmap fromIntegral $ throwErrnoIfMinus1RetryMayBlock "recvfrom"
                                                        (c_recvfrom s ptr (fromIntegral buflen) 0 sa_ptr salen_ptr)
                                                        (threadWaitRead (fromIntegral s))
  return (fromBS str, SA sa salen)

sendTo :: StringLike string => SocketAddress -> Socket -> string -> IO ()
sendTo (SA sa salen) (Socket s) str = do
  withForeignPtr sa $ \sa_ptr -> do
  B.unsafeUseAsCStringLen (toBS str) $ \(ptr,len) -> do
  r <- throwErrnoIfMinus1RetryMayBlock "sendTo" 
                                       (c_sendto s (castPtr ptr) (fromIntegral len) 0 sa_ptr (fromIntegral salen))
                                       (threadWaitWrite (fromIntegral s))
  when (r/=fromIntegral len) $ fail "sendTo: partial packet sent!"

foreign import CALLCONV unsafe "recv" c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO (#type ssize_t)
foreign import CALLCONV unsafe "send" c_send :: CInt -> Ptr Word8 -> CSize -> CInt -> IO (#type ssize_t)
foreign import CALLCONV unsafe "recvfrom" c_recvfrom :: CInt -> Ptr Word8 -> CSize -> CInt -> Ptr () -> Ptr SLen -> IO (#type ssize_t)
foreign import CALLCONV unsafe "sendto" c_sendto :: CInt -> Ptr Word8 -> CSize -> CInt -> Ptr () -> SLen -> IO (#type ssize_t)

-- | Close the socket specified.
closeSocket :: Socket -> IO ()
closeSocket (Socket fd) = throwErrnoIfMinus1_ "close" $ c_close fd

foreign import CALLCONV unsafe "bind"    c_bind    :: CInt -> Ptr () -> (SLen) -> IO CInt
foreign import CALLCONV unsafe "listen"  c_listen  :: CInt -> CInt -> IO CInt
foreign import CALLCONV unsafe "socket"  c_socket  :: CFamily -> CType -> CInt -> IO CInt
foreign import CALLCONV unsafe "connect" c_connect :: CInt -> Ptr () -> (SLen) -> IO CInt
#ifdef WINDOWS
foreign import CALLCONV unsafe "closesocket" c_close :: CInt -> IO CInt
#else
foreign import CALLCONV unsafe "close" c_close :: CInt -> IO CInt
#endif

newtype Socket = Socket CInt

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
  s   <- throwErrnoIfMinus1 "socket" $ c_socket fam stype 0
  setNonBlockingFD s
  let loop = do r   <- withForeignPtr sa $ \ptr -> c_connect s ptr (fromIntegral len)
	       	err <- getErrno
       	        case r of
                  -1 | err == eINTR       -> do loop
		     | err == eINPROGRESS -> do threadWaitWrite (fromIntegral s)
                                                soe <- getsockopt_error s
                                                if soe==0 then return (Socket s) else fail "connect"
                     |  otherwise         -> do fail "connect"
                  _                       -> do return $ Socket s
  loop

foreign import ccall unsafe getsockopt_error :: CInt -> IO CInt


-- | Get the family \(domain\) of the socket.
getFamily :: SocketAddress -> IO CFamily
getFamily (SA sa _) = worker >>= return . fromIntegral
    where worker :: IO #type sa_family_t
	  worker = withForeignPtr sa (#peek struct sockaddr, sa_family) 

csas :: forall a. (SocketAddress -> IO a) -> [SocketAddress] -> IO a
csas _ []       = fail "No such host"
csas c [sa]     = c sa
csas c (sa:sas) = do x <- try' (c sa)
                     case x of
                      (Left _)  -> csas c sas
                      (Right v) -> return v

try' :: IO a -> IO (Either SomeException a)
try' = E.try

withResolverLock :: IO a -> IO a
withResolverLock x = x

data SocketAddress = SA !(ForeignPtr ()) !Int deriving(Show)
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
a2sas _ _ (Unix fp)          = fail "Unix sockets not supported on Windows"
#else
a2sas _ _ (Unix fp)          = do let maxSize = ((#size struct sockaddr_un)-(#offset struct sockaddr_un, sun_path))
                                  when (length fp >= maxSize) $ fail "Too long address for Unix socket"
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
      uwork ai = do sal <- (#peek struct addrinfo, ai_addrlen)   ai
                    sa' <- (#peek struct addrinfo, ai_addr)      ai
                    sa  <- mallocForeignPtrBytes sal
                    copyBytes (unsafeForeignPtrToPtr sa) sa' sal
                    next<- (#peek struct addrinfo, ai_next)      ai
                    rest<- unai next
                    return ((SA sa sal):rest)
      getAI :: IO (Ptr AddrInfoT)
      getAI = allocaBytes (#size struct addrinfo) $ \hints -> do
              B.memset hints 0 (#size struct addrinfo)
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


throwGAIErrorIf :: IO CInt -> IO ()
throwGAIErrorIf comp = do 
  err <- comp
  when (err /= 0) (gaiError err >>= fail)

-- Don't use gai_strerror with winsock - it is not thread-safe there.
gaiError :: CInt -> IO String
#if WINDOWS

gaiError (#const EAI_AGAIN)    = return "Temporary failure in name resolution."
gaiError (#const EAI_BADFLAGS) = return "Invalid value for ai_flags."
gaiError (#const EAI_FAIL)     = return "Nonrecoverable failure in name resolution."
gaiError (#const EAI_FAMILY)   = return "The ai_family member is not supported."
gaiError (#const EAI_MEMORY)   = return "Memory allocation failure."
gaiError (#const EAI_NODATA)   = return "No address associated with nodename."
gaiError (#const EAI_NONAME)   = return "Neither nodename nor servname provided, or not known."
gaiError (#const EAI_SERVICE)  = return "The servname parameter is not supported for ai_socktype."
gaiError (#const EAI_SOCKTYPE) = return "The ai_socktype member is not supported."
gaiError x                     = return ("Unknown gai_error value "++show x)

#else

gaiError err = c_gai_strerror err >>= peekCString

foreign import CALLCONV unsafe "gai_strerror" c_gai_strerror :: CInt -> IO (Ptr CChar)

#endif


--hostNameToNumber :: Address -> IO [Address]
--hostNameToNumber = error "FIXME"
--hostNumberToName :: Address -> IO [Address]
--hostNumberToName = error "FIXME"

-- | Get the current hostname.
getCurrentHost :: IO HostName
getCurrentHost = do
  allocaArray 256 $ \buffer -> do
    throwErrnoIfMinus1_ "gethostname" $ c_gethostname buffer 256
    peekCString buffer

foreign import CALLCONV unsafe "gethostname" c_gethostname :: Ptr CChar -> CSize -> IO CInt

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
  when (null sas) $ fail "No address for server!"
  let sf ha psa = case threading ss of
                    Threaded -> forkIO (clo ha $ sfun ha psa) >> return ()
                    Inline   -> clo ha $ sfun ha psa
      clo ha = case closeConnection ss of
                True  -> \x -> x `E.finally` (hClose ha)
                False -> id
  forM sas $ \sa -> do
     fam  <- getFamily sa
     sock <- throwErrnoIfMinus1 "socket" $ c_socket fam sockStream 0
     setNonBlockingFD sock
     let socket = Socket sock
     let on :: CInt
         on = 1
         os = fromIntegral $ sizeOf on
     with on $ \onptr -> c_setsockopt sock (#const SOL_SOCKET) (#const SO_REUSEADDR) onptr os
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

foreign import CALLCONV unsafe "setsockopt" c_setsockopt ::
  CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt

-- | Bind a socket to an address. Be wary of AF_LOCAL + NFS blocking?
bind :: Socket -> SocketAddress -> IO ()
bind (Socket sock) (SA sa len) = do
  withForeignPtr sa $ \sa_ptr -> 
    throwErrnoIfMinus1_ "bind" $ c_bind sock sa_ptr (fromIntegral len)

-- | Listen on an socket
listen :: Socket -> Int -> IO ()
listen (Socket s) iv = throwErrnoIfMinus1_ "listen" (c_listen s (toEnum iv))

accept :: Socket -> SocketAddress -> IO (Socket, SocketAddress)
accept (Socket lfd) (SA _ len) = do
  sa <- mallocForeignPtrBytes len
  s  <- withForeignPtr sa $ \sa_ptr -> do
          with (fromIntegral len) $ \len_ptr -> do
            throwErrnoIfMinus1RetryMayBlock "accept" (c_accept lfd sa_ptr len_ptr) (threadWaitRead (fromIntegral lfd))
  setNonBlockingFD s
  return (Socket s,SA sa len)

foreign import CALLCONV unsafe "accept"  c_accept  :: CInt -> Ptr () -> Ptr (SLen) -> IO CInt

-- | Run a datagram (udp) server. The function does not block, use sleepForever if that is desired.
dgramServer  :: StringLike packet => ServerSpec -- ^ Server specification
             -> (packet -> Address -> IO [packet]) -- ^ The server function is given a received packet and the Address of the peer. It returns a list of reply packets to send to that peer. Note that the list elements are invidual packets, not concatenated together.
                -> IO [ThreadId] -- ^ ThreadIds of the server listener processes.
dgramServer ss sfun = do
  sas <- a2sas sockDgram (aiNumericserv .|. aiPassive) (address ss)
  when (null sas) $ fail "No address for server!"
  forM sas $ \sa -> do
     fam  <- getFamily sa
     sock <- throwErrnoIfMinus1 "socket" $ c_socket fam sockDgram 0
     setNonBlockingFD sock
     let socket = Socket sock
     let on :: CInt
         on = 1
         os = fromIntegral $ sizeOf on
     with on $ \onptr -> c_setsockopt sock (#const SOL_SOCKET) (#const SO_REUSEADDR) onptr os
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
  let ntop l a = allocaArray (fromIntegral l) $ \ptr -> do
                 r <- throwErrnoIfNull "inet_ntop" $ inet_ntop f a ptr l
                 peekCString r
  withForeignPtr sa $ \sa_ptr -> do
  case () of
    _ | f == afInet -> do n <- ntop (#const INET_ADDRSTRLEN)  $ (#ptr struct sockaddr_in, sin_addr) sa_ptr
                          p <- ntohs =<< (#peek struct sockaddr_in, sin_port) sa_ptr
                          return $ IPv4 n (fromIntegral p)
      | f == afInet6-> do n <- ntop (#const INET6_ADDRSTRLEN) $ (#ptr struct sockaddr_in6, sin6_addr) sa_ptr
                          p <- ntohs =<< (#peek struct sockaddr_in6, sin6_port) sa_ptr
                          return $ IPv6 n (fromIntegral p)
#ifndef WINDOWS
      | f == afLocal-> do n <- peekCString $ (#ptr struct sockaddr_un, sun_path) sa_ptr
                          return $ Unix n
#endif
      | otherwise   -> do fail "Unsupported address family!"

#include <arpa/inet.h>
foreign import CALLCONV unsafe inet_ntop :: CFamily -> Ptr a -> CString -> (SLen) -> IO CString
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
      | otherwise    -> do fail "Unsupported address family!"


type SLen = #type socklen_t
foreign import CALLCONV safe getnameinfo :: Ptr () -> SLen -> Ptr CChar -> SLen -> Ptr CChar -> SLen -> CInt -> IO CInt

-- | Sleep forever. Useful after a server.
sleepForever :: IO ()
sleepForever = threadDelay maxBound >> sleepForever

