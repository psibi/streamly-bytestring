{-# LANGUAGE CPP #-}

module Streamly.External.ByteString.Lazy
  ( read
  , fromArrayStream
  , toArrayStream
  )
where

import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.Word (Word8)
import Streamly.Internal.Memory.Array.Types (Array(..))
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
import Streamly.Internal.Data.Unfold (concat)
import Streamly.Internal.Data.Unfold.Types (Unfold(..))

import qualified Streamly.External.ByteString as Strict

import Streamly
import qualified Streamly.Prelude as S

import qualified Data.ByteString.Lazy.Internal as BSLI

import Prelude hiding (concat, read)

-- | Convert a serial stream of 'Array' 'Word8' to a lazy 'ByteString'.
{-# INLINE fromArrayStream #-}
fromArrayStream :: Monad m => SerialT m (Array Word8) -> m ByteString
fromArrayStream = S.foldr BSLI.chunk Empty . S.map Strict.fromArray

-- | Convert a lazy 'ByteString' to a serial stream of 'Array' 'Word8'.
{-# INLINE toArrayStream #-}
toArrayStream :: Monad m => ByteString -> SerialT m (Array Word8)
toArrayStream = S.unfold unf
  where
    unf = Unfold step seed
    seed = return
    step (Chunk bs bl) = return $ Yield (Strict.toArray bs) bl
    step Empty = return $ Stop

-- | Unfold a lazy Bytestring to a stream of Word8
{-# INLINE read #-}
read :: Monad m => Unfold m ByteString Word8
read = concat (Unfold step inject) Strict.read
  where
    inject = return
    step (Chunk h t) = return $ Yield h t
    step Empty = return $ Stop
