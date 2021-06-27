{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Streamly.External.ByteString
  ( toArray
  , fromArray

  , read
  , writeN
  , write
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
#if MIN_VERSION_base(4, 10, 0)
import Foreign.ForeignPtr (plusForeignPtr)
#else
import Foreign.ForeignPtr.Compat (plusForeignPtr)
#endif
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import GHC.Ptr (minusPtr, plusPtr)
import Streamly.Internal.Data.Array.Foreign.Type (Array(..))
import Streamly.Data.Unfold (Unfold, lmap)
import Streamly.Data.Fold (Fold)

import qualified Streamly.Internal.Data.Array.Foreign as A

import Prelude hiding (read)

-- | Convert a 'ByteString' to an array of 'Word8'. This function unwraps the
-- 'ByteString' and wraps it with 'Array' constructors and hence the operation
-- is performed in constant time.
{-# INLINE toArray #-}
toArray :: ByteString -> Array Word8
toArray (PS fp off len) = Array nfp endPtr
  where
    nfp = fp `plusForeignPtr` off
    endPtr = unsafeForeignPtrToPtr nfp `plusPtr` len

-- | Convert an array of 'Word8' to a 'ByteString'. This function unwraps the
-- 'Array' and wraps it with 'ByteString' constructors and hence the operation
-- is performed in constant time.
{-# INLINE fromArray #-}
fromArray :: Array Word8 -> ByteString
fromArray Array {..}
    | aLen == 0 = mempty
    | otherwise = PS aStart 0 aLen
  where
    aStartPtr = unsafeForeignPtrToPtr aStart
    aLen = aEnd `minusPtr` aStartPtr

-- | Unfold a strict ByteString to a stream of Word8.
{-# INLINE read #-}
read :: Monad m => Unfold m ByteString Word8
read = lmap toArray A.read

-- | Fold a stream of Word8 to a strict ByteString of given size in bytes.
{-# INLINE writeN #-}
writeN :: MonadIO m => Int -> Fold m Word8 ByteString
writeN i = fromArray <$> A.writeN i

-- | Fold a stream of Word8 to a strict ByteString of appropriate size.
{-# INLINE write #-}
write :: MonadIO m => Fold m Word8 ByteString
write = fromArray <$> A.write
