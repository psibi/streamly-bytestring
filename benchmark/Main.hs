module Main where

import Streamly
import Streamly.External.ByteString.Lazy
import Data.List (foldl')

import Data.Word (Word8)
import Streamly.External.ByteString
import Streamly.Internal.Memory.Array.Types (Array(..))

import qualified Streamly.Prelude as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Internal as BSL

import Gauge

bs :: Int -> BSL.ByteString
bs l = foldl' (flip (BSL.chunk . BS.singleton)) BSL.Empty $ take l $ cycle [0..255]

serialS :: Monad m => Int -> SerialT m (Array Word8)
serialS l = S.unfoldr step seed
  where
    seed = 0
    step x | x >= l = Nothing
           | otherwise = Just (toArray (BS.singleton (fromIntegral (x `mod` 255))), x + 1)

benchToArrayStream :: Int -> Benchmarkable 
benchToArrayStream x = nfAppIO (S.drain . toArrayStream) bsx
  where
    bsx = bs x

benchFromArrayStream :: Int -> Benchmarkable
benchFromArrayStream x = nfAppIO fromArrayStream sx
  where
    sx = serialS x

main :: IO ()
main = defaultMain
        [ bench "toArrayStream 100000 chunks" (benchToArrayStream 100000)
        , bench "fromArrayStream 100000 chunks" (benchFromArrayStream 100000)
        ]


