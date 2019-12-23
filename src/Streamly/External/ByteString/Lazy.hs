module Streamly.External.ByteString.Lazy
  ( readChunks
  , read
  
  , toChunks
  , fromChunks
  )
where

import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.Word (Word8)
import Streamly.Internal.Memory.Array.Types (Array(..))
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
import Streamly.Internal.Data.Unfold (concat)
import Streamly.Internal.Data.Unfold.Types (Unfold(..))

import qualified Streamly.External.ByteString as Strict
import qualified Streamly.Internal.Memory.Array as A

import Streamly
import qualified Streamly.Prelude as S

import qualified Data.ByteString.Lazy.Internal as BSLI

import Prelude hiding (concat, read)

-- | Unfold a lazy ByteString to a stream of 'Array' 'Words'.
{-# INLINE  readChunks #-}
readChunks :: Monad m => Unfold m ByteString (Array Word8)
readChunks = Unfold step seed
    where
    seed = return
    step (Chunk bs bl) = return $ Yield (Strict.toArray bs) bl
    step Empty = return $ Stop

-- | Unfold a lazy ByteString to a stream of Word8
{-# INLINE read #-}
read :: Monad m => Unfold m ByteString Word8
read = concat readChunks A.read

-- | Convert a lazy 'ByteString' to a serial stream of 'Array' 'Word8'.
{-# INLINE toChunks #-}
toChunks :: Monad m => ByteString -> SerialT m (Array Word8)
toChunks = S.unfold readChunks

-- | Convert a serial stream of 'Array' 'Word8' to a lazy 'ByteString'.
{-# INLINE fromChunks #-}
fromChunks :: Monad m => SerialT m (Array Word8) -> m ByteString
fromChunks = S.foldr BSLI.chunk Empty . S.map Strict.fromArray
