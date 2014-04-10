{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
module Data.Conduit.Serialization.Binary
  ( conduitDecode
  , conduitEncode
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
conduitEncode :: (Binary b, MonadThrow m) => Conduit b m ByteString
conduitEncode = CL.map put =$= conduitPut

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

-- | Runs putter repeatedly on a input stream.
conduitPut :: MonadThrow m => Conduit Put m ByteString
conduitPutGeneric(conduitPut, (sourcePut x $$ CL.mapM_ yield))

-- | Runs putter repeatedly on a input stream.
-- Returns a lazy butestring so it's possible to use vectorized
-- IO on the result either by calling' LBS.toChunks' or by 
-- calling 'Network.Socket.ByteString.Lazy.send'.
conduitPutLBS :: MonadThrow m => Conduit Put m LBS.ByteString
conduitPutGeneric(conduitPutLBS, yield (runPut x))

-- | Vectorized variant of 'conduitPut' returning list contains
-- all chunks from one element representation
conduitPutList :: MonadThrow m => Conduit Put m [ByteString]
conduitPutGeneric(conduitPutList, yield (LBS.toChunks (runPut x)))

-- | Vectorized variant of 'conduitPut'.
conduitPutMany :: MonadThrow m => Conduit Put m (V.Vector ByteString)
conduitPutGeneric(conduitPutMany, yield (V.fromList (LBS.toChunks (runPut x))))

-- | Create stream of strict bytestrings from 'Put' value.
sourcePut :: (MonadThrow m) => Put -> Producer m ByteString
sourcePut = CL.sourceList . LBS.toChunks . runPut

-- | Decode message from input stream.
sinkGet :: (Binary b, MonadThrow m) => Get b -> Consumer ByteString m b
sinkGet f = sink (runGetIncremental f)
  where
      sink (Done bs _ v)  = leftover bs >> return v
      sink (Fail u o e)   = monadThrow (ParseError u o e)
      sink (Partial next) = await >>= sink . next
