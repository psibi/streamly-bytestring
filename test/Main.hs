module Main where

import Control.Monad (filterM)
import qualified Data.ByteString as BS
import Streamly.ByteString
import qualified Streamly.ByteString.Lazy as SL
import Streamly.FileSystem.File (readArrays)
import System.Directory
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.ByteString

checkFileContent :: FilePath -> IO ()
checkFileContent filename = do
  print $ "Checking " <> filename
  bsContent <- BS.readFile filename
  let arrays = readArrays filename
  bsStreamly <- toByteString arrays
  bsContent `shouldBe` bsStreamly

getHundredFiles :: FilePath -> IO [FilePath]
getHundredFiles path = do
  files <- listDirectory path
  files2 <- filterM (\x -> doesFileExist $ path </> x) files
  pure $ take 100 $ map (\x -> path </> x) files2

main :: IO ()
main =
  hspec $ do
    describe "array functions" $ do
      it "arrayToByteString" $ do
        filenames <- getHundredFiles "/home/sibi"
        mapM_ checkFileContent filenames
      prop "identity function" $ \bs -> do
        arr <- fromByteString bs
        bs2 <- arrayToByteString arr
        bs `shouldBe` bs2
      prop "fromBytestring . toByteString == id" $ \bs -> do
        arr <- fromByteString bs
        bs2 <- toByteString (return arr)
        bs `shouldBe` bs2
      prop "(lazy) fromBytestring . toByteString == id" $ \bs -> do
        let arr = SL.fromByteString bs
        bs2 <- SL.toByteString arr
        bs `shouldBe` bs2
