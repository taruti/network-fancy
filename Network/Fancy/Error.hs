-----------------------------------------------------------------------------
--
-- Module      :  Network.Fancy.Error
-- Copyright   :  Taru Karttunen <taruti@taruti.net>
-- License     :  BSD3
--
-- Maintainer  :  taruti@taruti.net
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Network.Fancy.Error (
    NetworkException,
    throwGAIErrorIf,
    throwIfError,
    throwIfError_,
    throwNetworkException
) where

import Control.Exception
import Control.Monad
import Data.Typeable
import Foreign
import Foreign.C
import System.IO
import System.IO.Unsafe

-- | Exceptions occuring in network-fancy.
data NetworkException = NE !String Handle !Errno
                      | NE_GAI !CInt
                        deriving(Typeable)

instance Exception NetworkException

instance Eq NetworkException where
    (NE _ _ a) == (NE _ _ b) = a == b
    (NE_GAI a) == (NE_GAI b) = a == b
    _ == _ = False

instance Show NetworkException where
    show (NE s _ v) = s ++ ": " ++ strerror v
    show (NE_GAI v) = unsafePerformIO $ gaiError v


fixme = undefined

throwIfError_ :: String -> IO CInt -> IO ()
throwIfError_ desc act = throwIfError desc act >> return ()


throwIfError :: String -> IO CInt -> IO CInt
throwIfError desc act = do
    res <- act
    when (res == -1) (throwIO . NE desc fixme =<< getErrno)
    return res

throwNetworkException :: String -> Errno -> IO any
throwNetworkException desc err = throwIO $! NE desc fixme err



strerror :: Errno -> String
strerror (Errno val) = unsafePerformIO $
  allocaArray 512 $ \buffer -> do
    _ <- c_strerror_r val buffer 511
    peekCString buffer

foreign import ccall unsafe "strerror_r" c_strerror_r :: CInt -> Ptr CChar -> CSize -> IO CInt

throwGAIErrorIf :: IO CInt -> IO ()
throwGAIErrorIf comp = do
  err <- comp
  when (err /= 0) $ throwIO $ NE_GAI err

-- Don't use gai_strerror with winsock - it is not thread-safe there.
gaiError :: CInt -> IO String
#ifdef WINDOWS

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

