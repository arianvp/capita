module Main where

import Lib
import System.Timeout
import Network.Socket
import Control.Concurrent
import Control.Exception
import Control.Monad

main :: IO ()
main = bracket (socket AF_INET Stream defaultProtocol) sClose listener


listener sock = do
  addr <- inet_addr "127.0.0.1"
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 44444 addr)
  listen sock maxListenQueue
  forever $ do
    (h,addr) <- accept sock
    forkFinally (handleClient h) (\_ -> sClose h)


handleClient h = loop 600 4
  where
    loop time 0 = return ()
    loop time n = do
      x <- timeout time (tick h)
      case x of
        Nothing -> loop 600 (n-1)
        Just x  -> loop 600 4
      

tick h = putStrLn "hey"
  

  

