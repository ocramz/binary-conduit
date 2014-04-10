import Control.Monad (forM_, when)
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
import Control.Monad.Trans.Resource 

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

prop_part2 :: [Int] -> Property
prop_part2 xs = monadicIO $ do
    let m = BS.concat . Prelude.concatMap (LBS.toChunks . runPut . put) $ xs
    when (Prelude.length xs>0) $ do
        forM_ [0..BS.length m] $ \l -> do
            let (l1,l2) = BS.splitAt l m
            ma <- runExceptionT $ CL.sourceList [l1,l2]
                                $= conduitDecode
                                $$ CL.consume
            case ma of
                Left _  -> fail "exception in conduit"
                Right a -> stop (xs ?== a)

prop_part3 :: [Int] -> Property
prop_part3 xs = monadicIO $ do
    let m = BS.concat . Prelude.concatMap (LBS.toChunks . runPut . put) $ xs
    when (Prelude.length xs>0) $ do
      forM_ [1..BS.length m] $ \l -> do
          let (l1,l2) = BS.splitAt l m
          when (BS.length l2 > 0) $ do
            forM_ [1..BS.length l2] $ \l' -> do
                let (l2_1,l2_2) = BS.splitAt l' l2
                ma <- runExceptionT $ CL.sourceList [l1,l2_1,l2_2]
                                    $= conduitDecode
                                    $$ CL.consume
                case ma of
                    Left _ -> fail "exception in conduit"
                    Right a -> stop $ xs ?== a

main = hspec $ do
    describe "QC properties: conduitEncode =$= conduitDecode == id" $ do
        prop "int"               $ (prop_eq :: [Int] -> Property)
        prop "string"            $ (prop_eq :: [String] -> Property)
        prop "maybe int"         $ (prop_eq :: [Maybe Int] -> Property)
        prop "either int string" $ (prop_eq :: [Either Int String] -> Property)
        prop "(Int,Int)"         $ (prop_sink :: (Int,Int) -> Property)
        prop "(String,String)"   $ (prop_sink :: (String,String) -> Property)
    describe "QC properties partial lists" $ do
        prop "break data in 2 parts" $ (prop_part2)
        prop "break data in 3 parts" $ (prop_part3)
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
