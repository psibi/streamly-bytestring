{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.ByteString (ByteString)
import Data.Word (Word8)
import GHC.IO.Handle (Handle)
import System.Random (randomIO)
import Streamly.FileSystem.Handle (chunkReader)
import System.IO (openFile, IOMode(ReadMode))
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.ByteString ()

-- Internal imports
import Streamly.Internal.Data.Array.Type (Array(..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Streamly.External.ByteString as Strict
import qualified Streamly.External.ByteString.Lazy as Lazy
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Fold as Fold

streamToByteString :: Monad m => S.Stream m (Array Word8) -> m ByteString
streamToByteString stream =
    S.fold (Fold.foldl' (<>) mempty) $ fmap Strict.fromArray stream

checkFileContent :: FilePath -> Handle -> IO ()
checkFileContent filename handle' = do
    print $ "Checking " <> filename
    bsContent <- BS.hGetContents handle'
    handle <- openFile filename ReadMode
    bsStreamly <- streamToByteString $ S.unfold chunkReader handle
    bsContent `shouldBe` bsStreamly

word8List :: Int -> IO [Word8]
word8List l =
    S.fold Fold.toList $
    S.take l $
    fmap fromIntegral $
    fmap (\x -> abs x `mod` 256) $ S.repeatM (randomIO :: IO Int)

propFoldTestStrict :: [Word8] -> Spec
propFoldTestStrict bl =
    prop
        ("Strict: fold write . fromList = pack" ++
         " -- Size: " ++ show (length bl) ++ " bytes") $ do
        bl' <- fmap BS.unpack $ S.fold Strict.write $ S.fromList bl
        bl' `shouldBe` bl

propNFoldTestStrict :: Int -> [Word8] -> Spec
propNFoldTestStrict n bl =
    prop
        ("Strict: fold (writeN n) . fromList = pack . take n" ++
         " -- Size: " ++ show n ++ " bytes") $ do
        bl' <- fmap BS.unpack $ S.fold (Strict.writeN n) $ S.fromList bl
        bl' `shouldBe` take n bl

propUnfoldTestStrict :: [Word8] -> Spec
propUnfoldTestStrict bl =
    prop
        ("Strict: toList . unfold read . pack = id" ++
         " -- Size: " ++ show (length bl) ++ " bytes") $ do
        bl' <- S.fold Fold.toList (S.unfold Strict.reader (BS.pack bl))
        bl' `shouldBe` bl

propUnfoldTestLazy :: [Word8] -> Spec
propUnfoldTestLazy bl =
    prop
        ("Lazy: toList . unfold read . pack = id" ++
         " -- Size: " ++ show (length bl) ++ " bytes") $ do
        bl' <- S.fold Fold.toList (S.unfold Lazy.reader (BSL.pack bl))
        bl' `shouldBe` bl

propFromChunks :: Spec
propFromChunks =
    prop "Lazy.fromChunks = BSL.fromChunks" $ \aL -> do
        x1 <- Lazy.fromChunks $ fmap Strict.toArray $ S.fromList aL
        let x2 = BSL.fromChunks aL
        x1 `shouldBe` x2

propFromChunksIO :: Spec
propFromChunksIO =
    prop "Lazy.fromChunks = BSL.fromChunks" $ \aL -> do
        x1 <- Lazy.fromChunksIO $ fmap Strict.toArray $ S.fromList aL
        let x2 = BSL.fromChunks aL
        x1 `shouldBe` x2

testFromChunksIOLaziness :: Word8 -> IO Word8
testFromChunksIOLaziness h = do
    lbs <-
        Lazy.fromChunksIO $
        S.fromList (Strict.toArray (BS.singleton h) : undefined)
    return $ BSL.head lbs

main :: IO ()
main =
    hspec $ do
        describe "Array tests" $ do
            it "Strict fromArray" $
                mapM_
                    (flip withSystemTempFile checkFileContent . show)
                    ([1 .. 100] :: [Int])
            prop "Strict Identity" $ \bs ->
                bs `shouldBe` Strict.fromArray (Strict.toArray bs)
            prop "Strict Identity (with offset)" $ \bs ->
                let bs1 = BS.drop 5 bs
                 in bs1 `shouldBe` Strict.fromArray (Strict.toArray bs1)
            prop "toArray never produces negative length" $ \bs ->
                -- 'BS.drop 5' to trigger non-zero offset
                let (Array _ startI endI) = Strict.toArray (BS.drop 5 bs)
                 in (endI - startI) >= 0 `shouldBe` True
            prop "Lazy Identity" $ \bs -> do
                bs2 <- Lazy.fromChunks . Lazy.toChunks $ bs
                bs `shouldBe` bs2
        wl0 <- runIO $ word8List 0
        wlM <- runIO $ word8List 900
        wlL <- runIO $ word8List 73700
        describe "Strict: Fold tests" $ do
            propFoldTestStrict wl0
            propFoldTestStrict wlM
            propFoldTestStrict wlL
        describe "Strict: N Fold tests" $ do
            propNFoldTestStrict 0 wl0
            propNFoldTestStrict 900 wlM
            propNFoldTestStrict 73700 wlL
        describe "Strict: Unfold tests" $ do
            propUnfoldTestStrict wl0
            propUnfoldTestStrict wlM
            propUnfoldTestStrict wlL
        describe "Lazy: Unfold tests" $ do
            propUnfoldTestLazy wl0
            propUnfoldTestLazy wlM
            propUnfoldTestLazy wlL
        describe "Laziness of fromChunksIO" $ do
            it "Should not fail" $ do
                w <- testFromChunksIOLaziness 100
                w `shouldBe` 100
        describe "Correctness of fromChunks" propFromChunks
        describe "Correctness of fromChunksIO" propFromChunksIO
