import Control.Monad (forM_)
import Data.Binary
import Data.Binary.Put
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Serialization.Binary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Assertions
import Test.QuickCheck.Property
import Test.QuickCheck.Monadic
import Test.QuickCheck

-- | check conduitEncode =$= conduitDecode == id
prop_eq :: (Binary a,Eq a) => [a] -> Property
prop_eq xs = monadicIO $ do 
    xs' <- runExceptionT  $ CL.sourceList xs 
              $= enc xs
              =$= dec xs
              $$ CL.consume
    case xs' of
        Left e -> fail "exception"
        Right x -> assert $ x == xs
  where enc :: (Binary a, MonadThrow m) => [a] -> Conduit a m ByteString
        enc _ = conduitEncode 
        dec :: (Binary a, MonadThrow m) => [a] -> Conduit ByteString m a
        dec _ = conduitDecode

prop_sink :: (Binary a,Eq a) => (a,a) -> Property
prop_sink (a,b) = monadicIO $ do
    Right (a',b') <- runExceptionT $ CL.sourceList [a,b]
                         $= enc a
                         $$ do a' <- sinkGet get
                               b' <- CL.consume
                               return (a',b')

    assert $ a == a'
    assert $ runPut (put b) == LBS.fromChunks b'
  where enc :: (Binary a, MonadThrow m) => a -> Conduit a m ByteString
        enc _ = conduitEncode 
        dec :: (Binary a, MonadThrow m) => a -> Conduit ByteString m a
        dec _ = conduitDecode

prop_part :: [Int] -> Property
prop_part xs = monadicIO $ do
    let m = BS.concat . Prelude.concatMap (LBS.toChunks . runPut . put) $ xs
    forM_ [0..BS.length m] $ \l -> do
        let (l1,l2) = BS.splitAt l m
        Right a <- runExceptionT $ CL.sourceList [l1,l2]
                                 $= conduitDecode
                                 $$ CL.consume
        assert $ xs == a

main = hspec $ do
    describe "QC properties: conduitEncode =$= conduitDecode == id" $ do
        prop "int" $ (prop_eq :: [Int] -> Property)
        prop "string" $ (prop_eq :: [String] -> Property)
        prop "maybe int" $ (prop_eq :: [Maybe Int] -> Property)
        prop "either int string" $ (prop_eq :: [Either Int String] -> Property)
        prop "ab" $ (prop_sink :: (Int,Int) -> Property)
        prop "ab" $ (prop_sink :: (String,String) -> Property)
        prop "partial list" $ (prop_part)
    describe "HUnit properties:" $ do
      it "decodes message splitted to chunks" $ do
          let i = -32
              l = runPut (put (i::Int))
              (l1,l2) = LBS.splitAt (LBS.length l `div` 2) l
              t = BS.concat . LBS.toChunks
          x <- CL.sourceList [t l1,t l2] $= conduitDecode $$ CL.consume
          x `shouldBe` [i]
      it "decodes message with list of values inside" $ do
          let is = [-32,45::Int]
              ls = BS.concat . Prelude.concatMap (LBS.toChunks .runPut . put) $ is 
              (ls1,ls2) = BS.splitAt ((BS.length ls `div` 2) +1) ls
          x <- CL.sourceList [ls,ls] $= conduitDecode $$ CL.consume
          x' <- CL.sourceList [ls1,ls2] $= conduitDecode $$ CL.consume
          x `shouldBe` is++is
          x' `shouldBe` is
