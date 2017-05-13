{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
module Data.Conduit.Serialization.Binary
  ( conduitDecode
  , conduitEncode
  , conduitMsgEncode
  , conduitGet
  , conduitPut
  , conduitPutList
  , conduitPutLBS
  , conduitPutMany
  , sourcePut
  , sinkGet
  , ParseError(..)
  )
  where

import           Control.Exception
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import           Data.Conduit
import qualified Data.Conduit.List    as CL
import           Data.Typeable
import qualified Data.Vector          as V
import           Control.Monad.Trans.Resource 
     (MonadThrow
     , monadThrow)


data ParseError = ParseError
      { unconsumed :: ByteString
        -- ^ Data left unconsumed in single stream input value.

      , offset     :: ByteOffset
        -- ^ Number of bytes consumed from single stream input value.

      , content    :: String      -- ^ Error content.
      } deriving (Show, Typeable)

instance Exception ParseError

-- | Runs default 'Decoder' repeatedly on a input stream.
conduitDecode :: (Binary b, MonadThrow m) => Conduit ByteString m b
conduitDecode = conduitGet get

-- | Runs default encoder on a input stream.
--
-- This function produces a stream of bytes where for each input
-- value you will have a number of 'ByteString's, and no boundary
-- between different values. 
conduitEncode :: (Binary b, MonadThrow m) => Conduit b m ByteString
conduitEncode = CL.map put =$= conduitPut


-- | Runs default encoder on input stream.
--
-- This function produces a ByteString per each incomming packet,
-- it may be useful in datagram based protocols.
-- Function maintains following property
--
-- >   'conduitMsgEncode' xs == 'CL.map' 'Data.ByteString.encode' =$= 'CL.map' 'LBS.toStrict'
--
-- This invariant is maintaind by the cost of additional data copy,
-- so if you packets can be serialized to the large data chunks or
-- you interested in iterative packet serialization
-- concider using 'conduitPutList' or 'conduitPutMany'
--
conduitMsgEncode :: (Binary b, MonadThrow m) => Conduit b m ByteString
conduitMsgEncode = CL.map put =$= conduitMsg

-- | Runs getter repeatedly on a input stream.
conduitGet :: MonadThrow m => Get b -> Conduit ByteString m b
conduitGet g = start
  where
    start = do mx <- await
               case mx of
                  Nothing -> return ()
                  Just x -> go (runGetIncremental g `pushChunk` x)
    go (Done bs _ v) = do yield v
                          if BS.null bs
                            then start
                            else go (runGetIncremental g `pushChunk` bs)
    go (Fail u o e)  = monadThrow (ParseError u o e)
    go (Partial n)   = await >>= (go . n)

-- \o/
#define conduitPutGeneric(name,yi) \
name = conduit \
  where \
    conduit = do {mx <- await;\
                 case mx of;\
                    Nothing -> return ();\
                    Just x  -> do { yi ; conduit}}

-- | Runs putter repeatedly on a input stream, returns an output stream.
conduitPut :: Monad m => Conduit Put m ByteString
conduitPutGeneric(conduitPut, (sourcePut x $$ CL.mapM_ yield))

-- | Runs a putter repeatedly on a input stream, returns a packets.
conduitMsg :: Monad m => Conduit Put m ByteString
conduitPutGeneric(conduitMsg, (yield (LBS.toStrict $ runPut x)))

-- | Runs putter repeatedly on a input stream.
-- Returns a lazy butestring so it's possible to use vectorized
-- IO on the result either by calling' LBS.toChunks' or by 
-- calling 'Network.Socket.ByteString.Lazy.send'.
conduitPutLBS :: Monad m => Conduit Put m LBS.ByteString
conduitPutGeneric(conduitPutLBS, yield (runPut x))

-- | Vectorized variant of 'conduitPut' returning list contains
-- all chunks from one element representation
conduitPutList :: Monad m => Conduit Put m [ByteString]
conduitPutGeneric(conduitPutList, yield (LBS.toChunks (runPut x)))

-- | Vectorized variant of 'conduitPut'.
conduitPutMany :: Monad m => Conduit Put m (V.Vector ByteString)
conduitPutGeneric(conduitPutMany, yield (V.fromList (LBS.toChunks (runPut x))))

-- | Create stream of strict bytestrings from 'Put' value.
sourcePut :: Monad m => Put -> Producer m ByteString
sourcePut = CL.sourceList . LBS.toChunks . runPut

-- | Decode message from input stream.
sinkGet :: MonadThrow m => Get b -> Consumer ByteString m b
sinkGet f = sink (runGetIncremental f)
  where
      sink (Done bs _ v)  = leftover bs >> return v
      sink (Fail u o e)   = monadThrow (ParseError u o e)
      sink (Partial next) = await >>= sink . next
