module Streamly.External.ByteString.Lazy where

import Data.ByteString.Lazy.Internal (ByteString(..), foldlChunks)
import Data.Word (Word8)
import Streamly.External.ByteString as Strict
import Streamly.Internal.Memory.Array.Types (Array(..))
import Streamly

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.StreamD as D

{-# INLINE fromArrayStream #-}
fromArrayStream :: Monad m => SerialT m (Array Word8) -> m ByteString
fromArrayStream = S.foldl' (flip Chunk) Empty . S.map Strict.fromArray

{-# INLINE toArrayStream #-}
toArrayStream :: Monad m => ByteString -> SerialT m (Array Word8)
toArrayStream = D.fromStreamD . foldlChunks stepFunction D.nil
  where
    stepFunction s = flip D.cons s . Strict.toArray 
