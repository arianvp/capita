module Main where

import Lib
import System.Timeout
import Network.Socket
import Control.Concurrent
import Control.Exception
import Control.Monad

with s a = bracket s sClose  a

main :: IO ()
-- open a socket, and make sure it's closed properly after we exit
main = with (socket AF_INET Stream defaultProtocol) listener



listener sock = do
  addr <- inet_addr "127.0.0.1"
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 44444 addr)
  listen sock maxListenQueue
  forever $ do
    with (accept sock) $ \(h,addr) ->
      forkFinally (handleClient h) (const . sClose $ h)


handleClient h = loop 600 4
  where
    loop time 0 = return ()
    loop time n = do
      x <- timeout time (tick h)
      case x of
        Nothing -> loop 600 (n-1)
        Just x  -> loop 600 4
      

tick h = putStrLn "hey"
  

  

