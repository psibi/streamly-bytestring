{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.ByteString (ByteString)
import Data.Word (Word8)
import GHC.IO.Handle (Handle)
import System.Random (randomIO)
import Streamly
import Streamly.FileSystem.Handle (readChunks)
import Streamly.Memory.Array (Array)
import System.IO (openFile, IOMode(ReadMode))
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.ByteString ()

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Streamly.External.ByteString as Strict
import qualified Streamly.External.ByteString.Lazy as Lazy
import qualified Streamly.Prelude as S

streamToByteString :: MonadAsync m => SerialT m (Array Word8)-> m ByteString
streamToByteString stream = S.foldl' (<>) mempty $ S.map Strict.fromArray stream

checkFileContent :: FilePath -> Handle -> IO ()
checkFileContent filename handle' = do
  print $ "Checking " <> filename
  bsContent <- BS.hGetContents handle'
  handle <- openFile filename ReadMode
  bsStreamly <- streamToByteString $ S.unfold readChunks handle
  bsContent `shouldBe` bsStreamly

word8List :: Int -> IO [Word8]
word8List l = S.toList
       $ S.take l
       $ S.map fromIntegral
       $ S.map (\x -> abs x `mod` 256)
       $ S.repeatM (randomIO :: IO Int)

propFoldTestStrict :: [Word8] -> Spec
propFoldTestStrict bl =
  prop ("Strict: fold write . fromList = pack" ++ " -- Size: " ++ show (length bl) ++ " bytes") $ do
  bl' <- fmap BS.unpack $ S.fold Strict.write $ S.fromList bl
  bl' `shouldBe` bl

propFoldTestLazy :: [Word8] -> Spec
propFoldTestLazy bl =
  prop ("Lazy: fold write . fromList = pack" ++ " -- Size: " ++ show (length bl) ++ " bytes") $ do
  bl' <- fmap BSL.unpack $ S.fold Lazy.write $ S.fromList bl
  bl' `shouldBe` bl

propUnfoldTestStrict :: [Word8] -> Spec
propUnfoldTestStrict bl =
  prop ("Strict: toList . unfold read . pack = id" ++ " -- Size: " ++ show (length bl) ++ " bytes") $ do
  bl' <- S.toList (S.unfold Strict.read (BS.pack bl))
  bl' `shouldBe` bl

propUnfoldTestLazy :: [Word8] -> Spec
propUnfoldTestLazy bl =
  prop ("Lazy: toList . unfold read . pack = id" ++ " -- Size: " ++ show (length bl) ++ " bytes") $ do
  bl' <- S.toList (S.unfold Lazy.read (BSL.pack bl))
  bl' `shouldBe` bl

main :: IO ()
main =
  hspec $ do
    describe "Array tests" $ do
      it "Strict fromArray" $
        mapM_ (flip withSystemTempFile checkFileContent . show) ([1..100] :: [Int])
      prop "Strict Identity" $ \bs ->
        bs `shouldBe` Strict.fromArray (Strict.toArray bs)
    wl0 <- runIO $ word8List 0
    wlM <- runIO $ word8List 900    
    wlL <- runIO $ word8List 73700
    describe "Strict: Fold tests" $ do      
      propFoldTestStrict wl0
      propFoldTestStrict wlM
      propFoldTestStrict wlL
    describe "Lazy: Fold tests" $ do      
      propFoldTestLazy wl0
      propFoldTestLazy wlM
      propFoldTestLazy wlL
    describe "Strict: Unfold tests" $ do
      propUnfoldTestStrict wl0
      propUnfoldTestStrict wlM
      propUnfoldTestStrict wlL
    describe "Lazy: Unfold tests" $ do
      propUnfoldTestLazy wl0
      propUnfoldTestLazy wlM
      propUnfoldTestLazy wlL

        


