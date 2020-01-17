{-# LANGUAGE CPP #-}

module Streamly.External.ByteString.Lazy
  (fromArrayStream, toArrayStream)
where

import Data.ByteString.Lazy.Internal (ByteString(..), foldlChunks)
import Data.Word (Word8)
import Streamly.External.ByteString as Strict
import Streamly.Internal.Memory.Array.Types (Array(..))
import Streamly

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

-- Convert a serial stream of 'Array' 'Word8' to a lazy 'ByteString'.
{-# INLINE fromArrayStream #-}
fromArrayStream :: Monad m => SerialT m (Array Word8) -> m ByteString
fromArrayStream = S.foldl' (flip Chunk) Empty . S.map Strict.fromArray

-- Convert a lazy 'ByteString' to a serial stream of 'Array' 'Word8'.
{-# INLINE toArrayStream #-}
toArrayStream :: Monad m => ByteString -> SerialT m (Array Word8)
toArrayStream = D.fromStreamD . foldlChunks stepFunction nil
  where
    stepFunction s = flip cons s . Strict.toArray 

#define INLINE_EARLY  INLINE [2]
#define INLINE_NORMAL INLINE [1]
#define INLINE_LATE   INLINE [0]

-- | An empty 'Stream'.
{-# INLINE_NORMAL nil #-}
nil :: Monad m => D.Stream m a
nil = D.Stream (\_ _ -> return D.Stop) ()

-- | Can fuse but has O(n^2) complexity.
{-# INLINE_NORMAL cons #-}
cons :: Monad m => a -> D.Stream m a -> D.Stream m a
cons x (D.Stream step state) = D.Stream step1 Nothing
    where
    {-# INLINE_LATE step1 #-}
    step1 _ Nothing   = return $ D.Yield x (Just state)
    step1 gst (Just st) = do
        r <- step gst st
        return $
          case r of
            D.Yield a s -> D.Yield a (Just s)
            D.Skip  s   -> D.Skip (Just s)
            D.Stop      -> D.Stop

            


