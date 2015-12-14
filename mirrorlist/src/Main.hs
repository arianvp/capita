module Main where

import Control.Concurrent
import Control.Monad.Par.IO
import Control.Monad.Par.Class
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception
import Network.Stream
import Network.HTTP
import Network.URI
import Data.ByteString hiding (putStrLn, reverse)
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import System.Environment


-- simply execute for its side effect
getURI uri = liftIO $ 
  simpleHTTP (Request uri GET [] (mempty::ByteString))



parseURIs :: [String] -> Maybe [URI]
parseURIs = mapM parseURI

-- this doesn't work as it will preserve order. not what we want
  

-- we can't use concurrently either because it's order preserving
-- we want somehting like concurrently but with race
--
-- We want the following semantics:
--
-- the resulting [URI] should be sorted by speed, (first completion),
-- and omit any URI that wasn't reached

sortURIsSTM :: [URI] -> IO [URI]
sortURIsSTM xs = do
  res <- newTVarIO []
  mapConcurrently (getURI' res) xs
  x <- readTVarIO res
  return (reverse x)
  where
    getURI' res uri = (do
      x <- getURI uri
      case x of
        Right (Response e _ _  _) -> do
            print e
            if e < (4,0,0)
              then atomically $ modifyTVar res (uri:)
              else return ())
          `catch` \(SomeException e) -> return ()

      
        

    
    
      
main :: IO ()
main = do
  args <- getArgs
  case parseURIs args of
    Just urls -> do
      urls' <- sortURIsSTM urls
      mapM_ print urls'
    Nothing ->
      putStrLn "WTF Error"
      
  
