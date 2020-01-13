{-# LANGUAGE RecordWildCards #-}

module Streamly.External.ByteString
  (fromArray, toArray)
where

import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (plusForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import GHC.Ptr (castPtr, minusPtr, plusPtr, Ptr(..))
import Streamly.Internal.Memory.Array.Types (Array(..))

import qualified Streamly.Prelude as S

-- Convert an array of 'Word8' to a 'ByteString'. This function unwraps the
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

-- Convert a 'ByteString' to an array of 'Word8'. This function unwraps the
-- 'ByteString' and wraps it with 'Array' constructors and hence the operation
-- is performed in constant time. 
{-# INLINE toArray #-}
toArray :: ByteString -> Array Word8
toArray (PS fp off len) = Array nfp endPtr endPtr
  where
    nfp = fp `plusForeignPtr` off
    endPtr = unsafeForeignPtrToPtr fp `plusPtr` len
