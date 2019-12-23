{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (filterM)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Streamly
import Streamly.Memory.Array (Array)
import System.Directory
import System.FilePath ((</>))
import Streamly.FileSystem.Handle (readChunks)
import Test.Hspec
import Test.Hspec.QuickCheck
import System.IO (openFile, IOMode(ReadMode))
import Test.QuickCheck.Instances.ByteString

import qualified Data.ByteString as BS
import qualified Streamly.ByteString as Strict
import qualified Streamly.ByteString.Lazy as Lazy
import qualified Streamly.Prelude as S

streamToByteString :: MonadAsync m => SerialT m (Array Word8)-> m ByteString
streamToByteString stream = S.foldl' (<>) mempty $ S.map Strict.fromArray stream

checkFileContent :: FilePath -> IO ()
checkFileContent filename = do
  print $ "Checking " <> filename
  bsContent <- BS.readFile filename
  handle <- openFile filename ReadMode
  bsStreamly <- streamToByteString $ S.unfold readChunks handle
  bsContent `shouldBe` bsStreamly

getHundredFiles :: FilePath -> IO [FilePath]
getHundredFiles path = do
  files <- listDirectory path
  files2 <- filterM (\x -> doesFileExist $ path </> x) files
  return $ take 100 $ map (\x -> path </> x) files2

main :: IO ()
main =
  hspec $ do
    describe "array functions" $ do
      it "Strict fromArray" $ do
        filenames <- getHundredFiles "/home/creed/"
        mapM_ checkFileContent filenames
      prop "Strict Identity" $ \bs ->
        bs `shouldBe` Strict.fromArray (Strict.toArray bs)
      prop "Lazy Identity" $ \bs -> do
        bs2 <- Lazy.fromArray . Lazy.toArray $ bs
        bs `shouldBe` bs2
