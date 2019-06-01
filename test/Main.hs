module Main where

import Control.Monad (filterM)
import qualified Data.ByteString as BS
import Streamly.ByteString
import Streamly.FileSystem.File (readArrays)
import System.Directory
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck

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
