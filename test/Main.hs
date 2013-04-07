import Data.Binary
import Data.Binary.Put
import Data.ByteString
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

main = hspec $ describe "conduitEncode =$= conduitDecode == id" $ do
    prop "int" $ (prop_eq :: [Int] -> Property)
    prop "string" $ (prop_eq :: [String] -> Property)
    prop "maybe int" $ (prop_eq :: [Maybe Int] -> Property)
    prop "either int string" $ (prop_eq :: [Either Int String] -> Property)
    prop "ab" $ (prop_sink :: (Int,Int) -> Property)
    prop "ab" $ (prop_sink :: (String,String) -> Property)
    
