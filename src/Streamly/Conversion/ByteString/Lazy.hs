module Streamly.Conversion.ByteString.Lazy where

import Data.ByteString.Lazy (ByteString, foldlChunks, fromChunks)
import Data.Word (Word8)
import Streamly.Conversion.ByteString as Strict
import Streamly.Internal.Memory.Array.Types (Array(..))
import Streamly

import qualified Streamly.Prelude as S

fromArray :: Monad m => SerialT m (Array Word8) -> m ByteString
fromArray = fmap fromChunks . S.toList . S.map Strict.fromArray

toArray :: Monad m => ByteString -> SerialT m (Array Word8)
toArray = foldlChunks stepFunction S.nil
  where
    stepFunction s b = s <> return (Strict.toArray b) 
