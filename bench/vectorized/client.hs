-- | Author:     pxqr.sta@gmail.com
-- Set of benchmark for vectorized output
module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Criterion.Main
import System.Environment
import Network.Socket
import Network.Socket.ByteString
import Data.Binary
import Data.Binary.Put

data Message = Tiny !Int
             | Mid  !ByteString

instance Binary Message where
  get = error "Binary.Message.get: not implemented"
  put (Tiny i) = do
    putWord32be 5 -- size
    putWord8 0x00 -- index
    putWord32be (fromIntegral i) -- value

  put (Mid bs) = do
    putWord32be $ fromIntegral $ 5 + BS.length bs -- size
    putWord8 0x01 -- index
    putByteString bs            -- value

-- current version
sendChunks :: Socket -> [Message] -> IO ()
sendChunks sock msgs = mapM_ (sendAll sock) $ concatMap (LBS.toChunks . encode) msgs

-- original issue version
sendVectored :: Socket -> [Message] -> IO ()
sendVectored sock msgs = sendMany sock $ concatMap (LBS.toChunks . encode) msgs

-- current version with coalescing of small blocks by binary
-- we can use this as somewhat similar to buffering
sendCoalesced :: Socket -> [Message] -> IO ()
sendCoalesced sock msgs = mapM_ (sendAll sock) $ LBS.toChunks $ runPut $ mapM_ put msgs

-- new issue version
sendCoalescedVectored :: Socket -> [Message] -> IO ()
sendCoalescedVectored sock msgs = sendMany sock $ LBS.toChunks $ runPut $ mapM_ put msgs

-- adaptive version
sendAdaptive :: Socket -> [Message] -> IO ()
sendAdaptive sock msgs = do
  case LBS.toChunks $ runPut $ mapM_ put msgs of
    []   -> return ()
    [bs] -> sendAll  sock bs
    xs   -> sendMany sock xs

main :: IO ()
main = do
  portStr : crit <- getArgs
  let port = fromIntegral $ read portStr
  sock <- socket AF_INET Stream defaultProtocol
  connect sock (SockAddrInet port 0)

  withArgs crit $ do
    defaultMain
      [ -- on tiny messages we get 20 times boost from coalescing;

        -- 9KB
        bench "tiny-1K/chunk"                 $ nfIO $ sendChunks            sock $ replicate 1024     (Tiny 0) -- 4MB/sec
      , bench "tiny-1K/vectored"              $ nfIO $ sendVectored          sock $ replicate 1024     (Tiny 0) -- 4MB/sec
      , bench "tiny-1K/chunk-coalesced"       $ nfIO $ sendCoalesced         sock $ replicate 1024     (Tiny 0) -- 100MB/sec
      , bench "tiny-1K/vectored-coalesced"    $ nfIO $ sendCoalescedVectored sock $ replicate 1024     (Tiny 0) -- 100MB/sec
      , bench "tiny-1K/adaptive"              $ nfIO $ sendAdaptive          sock $ replicate 1024     (Tiny 0) -- 100MB/sec

        -- 90KB
      , bench "tiny-10K/chunk"                 $ nfIO $ sendChunks            sock $ replicate (10 * 1024) (Tiny 0) -- 3.2MB/sec
      , bench "tiny-10K/vectored"              $ nfIO $ sendVectored          sock $ replicate (10 * 1024) (Tiny 0) -- 2.5MB/sec
      , bench "tiny-10K/chunk-coalesced"       $ nfIO $ sendCoalesced         sock $ replicate (10 * 1024) (Tiny 0) -- 80MB/sec
      , bench "tiny-10K/vectored-coalesced"    $ nfIO $ sendCoalescedVectored sock $ replicate (10 * 1024) (Tiny 0) -- 80MB/sec
      , bench "tiny-10K/adaptive"              $ nfIO $ sendAdaptive          sock $ replicate (10 * 1024) (Tiny 0) -- 80MB/sec

        -- 900KB
        -- without coalescing this two goes too slow
--      , bench "tiny-100/chunk"                 $ nfIO $ sendChunks            sock $ replicate (100 * 1024) (Tiny 0)
--      , bench "tiny-100/vectored"              $ nfIO $ sendVectored          sock $ replicate (100 * 1024) (Tiny 0)
      , bench "tiny-100K/chunk-coalesced"       $ nfIO $ sendCoalesced         sock $ replicate (100 * 1024) (Tiny 0) -- 80MB/sec
      , bench "tiny-100K/vectored-coalesced"    $ nfIO $ sendCoalescedVectored sock $ replicate (100 * 1024) (Tiny 0) -- 80MB/sec
      , bench "tiny-100K/adaptive"              $ nfIO $ sendAdaptive          sock $ replicate (100 * 1024) (Tiny 0) -- 80MB/sec


        ------------------------------------------------------------------------------------------
        -- all benchmarks below send 1 MB with different size of chunks

        -- send 20 chunks: <5 byte header> with <160KB payload> interspersed
        -- all versions gives 600 MB/sec
      , bench "mid/10/chunk"                 $ nfIO $
          sendChunks            sock $ replicate 10     $ Mid $ BS.replicate (100 * 1024) 0
      , bench "mid/10/vectored"              $ nfIO $
          sendVectored          sock $ replicate 10     $ Mid $ BS.replicate (100 * 1024) 0
      , bench "mid/10/chunk-coalesced"       $ nfIO $
          sendCoalesced         sock $ replicate 10     $ Mid $ BS.replicate (100 * 1024) 0
      , bench "mid/10/vectored-coalesced"    $ nfIO $
          sendCoalescedVectored sock $ replicate 10     $ Mid $ BS.replicate (100 * 1024) 0
      , bench "mid/10/adaptive"              $ nfIO $
          sendAdaptive          sock $ replicate 10     $ Mid $ BS.replicate (100 * 1024) 0

        -- send 200 chunks: <5 byte header> with <16KB payload> interspersed
        -- all versions gives 600 MB/sec
      , bench "mid/100/chunk"                 $ nfIO $
          sendChunks            sock $ replicate 100     $ Mid $ BS.replicate (10 * 1024) 0
      , bench "mid/100/vectored"              $ nfIO $
          sendVectored          sock $ replicate 100     $ Mid $ BS.replicate (10 * 1024) 0
      , bench "mid/100/chunk-coalesced"       $ nfIO $
          sendCoalesced         sock $ replicate 100     $ Mid $ BS.replicate (10 * 1024) 0
      , bench "mid/100/vectored-coalesced"    $ nfIO $
          sendCoalescedVectored sock $ replicate 100     $ Mid $ BS.replicate (10 * 1024) 0
      , bench "mid/100/adaptive"              $ nfIO $
          sendAdaptive          sock $ replicate 100     $ Mid $ BS.replicate (10 * 1024) 0

      , bench "mid/1000/chunk"                 $ nfIO $
          sendChunks            sock $ replicate 1000     $ Mid $ BS.replicate 1024 0
      , bench "mid/1000/vectored"              $ nfIO $
          sendVectored          sock $ replicate 1000     $ Mid $ BS.replicate 1024 0
      , bench "mid/1000/chunk-coalesced"       $ nfIO $
          sendCoalesced         sock $ replicate 1000     $ Mid $ BS.replicate 1024 0
      , bench "mid/1000/vectored-coalesced"    $ nfIO $
          sendCoalescedVectored sock $ replicate 1000     $ Mid $ BS.replicate 1024 0
      , bench "mid/1000/adaptive"              $ nfIO $
          sendAdaptive          sock $ replicate 1000     $ Mid $ BS.replicate 1024 0

          -- 30MB/sec
      , bench "mid/10000/chunk"                 $ nfIO $
          sendChunks            sock $ replicate 10000     $ Mid $ BS.replicate 102 0

          -- 10MB/sec
      , bench "mid/10000/vectored"              $ nfIO $
          sendVectored          sock $ replicate 10000     $ Mid $ BS.replicate 102 0

          -- 50MB/sec
      , bench "mid/10000/chunk-coalesced"       $ nfIO $
          sendCoalesced         sock $ replicate 10000     $ Mid $ BS.replicate 102 0

          -- 20MB/sec
      , bench "mid/10000/vectored-coalesced"    $ nfIO $
          sendCoalescedVectored sock $ replicate 10000     $ Mid $ BS.replicate 102 0

          -- 20MB/sec
      , bench "mid/10000/adaptive"              $ nfIO $
          sendAdaptive          sock $ replicate 10000     $ Mid $ BS.replicate 102 0
      ]

  close sock
