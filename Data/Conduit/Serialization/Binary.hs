{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
module Data.Conduit.Serialization.Binary
  ( conduitDecode
  , conduitEncode
  , conduitGet
  , conduitPut
  , sourcePut
  , sinkGet
  , ParseError(..)
  )
  where

import           Control.Exception
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

import           Data.Conduit
import qualified Data.Conduit.List    as CL
import           Data.Typeable


data ParseError = ParseError
      { uncomsumed :: ByteString  -- ^ uncomsumed data
      , offset     :: ByteOffset  -- ^ current offset
      , content    :: String      -- ^ error content
      } deriving (Show, Typeable)

instance Exception ParseError

-- | Runs default decoder repeadetly on a input stream
conduitDecode :: (Binary b, MonadThrow m) => Conduit ByteString m b
conduitDecode = conduitGet get

-- | Runs default encoder on a input stream
conduitEncode :: (Binary b, MonadThrow m) => Conduit b m ByteString
conduitEncode = CL.map put =$= conduitPut

-- | Runs getter repeadetelly on a input stream
conduitGet :: (Binary b, MonadThrow m) => Get b -> Conduit ByteString m b
conduitGet g = start
  where 
    start = do mx <- await
               case mx of
                  Nothing -> return ()
                  Just x -> conduit (runGetIncremental g `pushChunk` x)
    conduit p = await >>= go . flip (maybe pushEndOfInput (flip pushChunk)) p
        where
          go (Done bs _ v) = do yield v
                                go (runGetIncremental get `pushChunk` bs)
          go (Fail u o e)  = monadThrow (ParseError u o e)
          go (Partial _)   = start

-- | Runs putter repeadelty on a input stream
conduitPut :: MonadThrow m => Conduit Put m ByteString
conduitPut = conduit 
  where 
    conduit = do mx <- await
                 case mx of
                     Nothing -> return ()
                     Just x  -> do sourcePut x $$ CL.mapM_ yield
                                   conduit

sourcePut :: (MonadThrow m) => Put -> Producer m ByteString
sourcePut = CL.sourceList . LBS.toChunks . runPut

sinkGet :: (Binary b, MonadThrow m) => Get b -> Consumer ByteString m b
sinkGet f = sink (runGetIncremental f)
  where
      sink (Done bs _ v)  = leftover bs >> return v
      sink (Fail u o e)   = monadThrow (ParseError u o e)
      sink (Partial next) = await >>= sink . next
