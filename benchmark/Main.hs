module Main where

import qualified Streamly.External.ByteString as Strict
import qualified Streamly.External.ByteString.Lazy as Lazy
import Control.Monad.IO.Class (MonadIO)

import qualified Streamly.Prelude as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Control.DeepSeq (NFData(..))
import System.Random (randomRIO)

import Gauge

numElements :: Int
numElements = 100000

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO a) -> (a -> b) -> Benchmark
benchIO name src f = bench name $ nfIO $
    randomRIO (1,1) >>= src >>= return . f

{-# INLINE benchIO' #-}
benchIO' :: NFData b => String -> (Int -> IO a) -> (a -> IO b) -> Benchmark
benchIO' name src f = bench name $ nfIO $
    randomRIO (1,1) >>= src >>= f

{-# INLINE strictWrite #-}
strictWrite :: MonadIO m => Int -> m BS.ByteString
strictWrite n = S.fold Strict.write
                $ S.map fromIntegral
                $ S.map (\x -> x `mod` 256)
                $ S.enumerateFromTo n (n + numElements)

{-# INLINE lazyWrite #-}
lazyWrite :: MonadIO m => Int -> m BSL.ByteString
lazyWrite n = S.fold Lazy.write 
              $ S.map fromIntegral
              $ S.map (\x -> x `mod` 256)
              $ S.enumerateFromTo n (n + numElements)

{-# INLINE lazyWrite' #-}
lazyWrite' :: MonadIO m => Int -> Int -> m BSL.ByteString
lazyWrite' chunkSize n = S.fold (Lazy.write' chunkSize)
              $ S.map fromIntegral
              $ S.map (\x -> x `mod` 256)
              $ S.enumerateFromTo n (n + numElements)

{-# INLINE strictRead #-}
strictRead :: MonadIO m => BS.ByteString -> m ()
strictRead = S.drain . S.unfold Strict.read

{-# INLINE lazyRead #-}
lazyRead :: MonadIO m => BSL.ByteString -> m ()
lazyRead = S.drain . S.unfold Lazy.read

main :: IO ()
main = defaultMain
        [ benchIO "Strict Write" strictWrite id
        , benchIO' "Strict Read" strictWrite strictRead
        , benchIO "Lazy Write" lazyWrite id
        , benchIO' "Lazy Read" lazyWrite lazyRead        
        , benchIO "Lazy Write with chunk size as 1" (lazyWrite' 1) id
        , benchIO' "Lazy Read with chunk size as 1" (lazyWrite' 1) lazyRead        
        ]


