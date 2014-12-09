import Control.Concurrent
import Control.Exception
import Data.Typeable
import Network.Fancy
import System.IO
import System.IO.Unsafe
import System.Directory
import System.Random

main = do
  ipv4_test
  ipv6_test
  unix_test
  put "> connect tests"
  tryE $ connectStream $ IPv4 "google.com" 80
  tryE $ connectStream $ IPv4 "foobarbaz.invalid" 0
  tryE $ connectDgram  $ IPv4 "foobarbaz.invalid" 0
  tryE $ connectStream $ IP   "foobarbaz.invalid" 0
  put "> done"
  pri =<< getCurrentHost
  put ">testing instances"
  let a = IP "" 0
  pri a
  pri (a == a)
  pri (a < a)
  put "> dgram server"
  tryE $ do
    addr <- IPv4 "127.0.0.1" `fmap` rport
    let rev :: String -> String
        rev = reverse
    dgramServer (serverSpec { address = addr, reverseAddress = ReverseName }) (\s sa -> put ("< connect from "++show sa) >> return [rev s])
    withDgram addr $ \s -> do
    send s "PING"
    "GNIP" <- recv s 99
    return ()

rport = randomRIO (2000,50000)

ipv4_test = server_test =<< (IPv4 "127.0.0.1" `fmap` rport)
ipv6_test = server_test =<< (IPv6 "::1" `fmap` rport)

unix_test = do
  tryE $ removeFile "/tmp/unix_test"
  server_test $ Unix "/tmp/unix_test"

server_test adr = tryE $ do
  put ("> running server_test "++show adr)
  streamServer (serverSpec { address = adr }) (\h sa -> put ("< connect from "++show sa) >> hGetLine h >>= hPutStrLn h . reverse)
  put "> starting client"
  withStream adr $ \h -> do
  put ("> client to "++show adr)
  hPutStrLn h "PING"
  hFlush h
  "GNIP" <- hGetLine h
  put "> ok"

tryE :: IO a -> IO ()
tryE x = try x >>= eh
eh :: Either SomeException a -> IO ()
eh (Left e) = put ("FAILURE: "++ show e)
eh _        = return ()


logLock = unsafePerformIO $ newMVar ()
pri x = withMVar logLock $ \_ -> print x
put x = withMVar logLock $ \_ -> putStrLn x

