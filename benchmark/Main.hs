module Main where

import Streamly
import Streamly.Memory.ByteString.Lazy
import Control.DeepSeq (rnf)

import qualified Streamly.Prelude as S
import qualified Data.ByteString.Lazy as BSL

import Gauge

bs :: Int -> BSL.ByteString
bs l =
    BSL.unfoldr
        (\x ->
             if x < l
                 then Just (fromIntegral (x `mod` 255), x + 1)
                 else Nothing)
        (0 :: Int)

benchToArrayStream :: Int -> Benchmarkable 
benchToArrayStream x =
    case rnf bsx of
        () -> nfAppIO (S.drain . toArrayStream) bsx
  where
    bsx = bs x

main :: IO ()
main = defaultMain
        [ bench "toArrayStream 100000" (benchToArrayStream 100000)
        ]


