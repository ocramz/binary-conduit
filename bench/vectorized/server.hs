-- | Author:     pxqr.sta@gmail.com
-- Set of benchmark for vectorized output
module Main (main) where

import Control.Monad
import Control.Exception
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import System.Environment


main :: IO ()
main = do
  port <- fmap (fromIntegral . read . head) getArgs
  bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
    bindSocket sock (SockAddrInet port 0)
    listen sock 1
    putStrLn "listenning"

    bracket (fmap fst (accept sock)) close $ \conn -> do
      forever $ do
         recv conn (4 * 1024)
