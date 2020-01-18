{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.ByteString (ByteString)
import Data.Word (Word8)
import GHC.IO.Handle (Handle)
import Streamly
import Streamly.FileSystem.Handle (readChunks)
import Streamly.Memory.Array (Array)
import System.IO (openFile, IOMode(ReadMode))
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.ByteString ()

import qualified Data.ByteString as BS
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

main :: IO ()
main =
  hspec $
    describe "Array tests" $ do
      it "Strict fromArray" $
        mapM_ (flip withSystemTempFile checkFileContent . show) ([1..100] :: [Int])
      prop "Strict Identity" $ \bs ->
        bs `shouldBe` Strict.fromArray (Strict.toArray bs)
      prop "Lazy Identity" $ \bs -> do
        bs2 <- Lazy.fromArrayStream . Lazy.toArrayStream $ bs
        bs `shouldBe` bs2
