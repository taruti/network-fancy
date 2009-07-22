import Control.Exception
import Data.Typeable
import Network.Fancy
import System.IO
import System.Directory
import System.Random

main = do
  ipv4_test
  ipv6_test
  unix_test
  putStrLn "> connect tests"
  tryE $ connectStream $ IPv4 "google.com" 80
  tryE $ connectStream $ IPv4 "foobarbaz.invalid" 0
  tryE $ connectDgram  $ IPv4 "foobarbaz.invalid" 0
  tryE $ connectStream $ IP   "foobarbaz.invalid" 0
  putStrLn "> done"
  print =<< getCurrentHost
  putStrLn ">testing instances"
  let a = IP "" 0
  print a
  print (a == a)
  print (a < a)
  putStrLn "> dgram server"
  tryE $ do
    addr <- IPv4 "127.0.0.1" `fmap` rport
    let rev :: String -> String
        rev = reverse
    dgramServer (serverSpec { address = addr, reverseAddress = ReverseName }) (\s sa -> putStrLn ("< connect from "++show sa) >> return [rev s])
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
  putStrLn ("> running server_test "++show adr)
  streamServer (serverSpec { address = adr }) (\h sa -> putStrLn ("< connect from "++show sa) >> hGetLine h >>= hPutStrLn h . reverse)
  putStrLn "> starting client"
  withStream adr $ \h -> do
  putStrLn ("> client to "++show adr)
  hPutStrLn h "PING"
  hFlush h
  "GNIP" <- hGetLine h
  putStrLn "> ok"

tryE :: IO a -> IO ()
tryE x = try x >>= eh
eh :: Either SomeException a -> IO ()
eh (Left e) = print e
eh _        = return ()

