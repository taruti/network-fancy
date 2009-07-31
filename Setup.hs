#!/usr/bin/env runhaskell
import Distribution.Simple
import System.Cmd
import System.Directory

main = defaultMainWithHooks simpleUserHooks{ runTests = \_ _ _ _ -> rt }

rt :: IO ()
rt = do system "ghc --make -threaded test.hs -o test"
        system "./test"
        mapM_ removeFile ["test","test.hi","test.o"]
        return ()
