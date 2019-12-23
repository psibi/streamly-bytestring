{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.ByteString where

import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (plusForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import GHC.Ptr (castPtr, minusPtr, plusPtr, Ptr(..))
import Streamly.Internal.Memory.Array.Types (Array(..))

import qualified Streamly.Prelude as S

fromArray :: Array Word8 -> ByteString
fromArray Array {..}
  | aLen == 0 = mempty
  | otherwise = PS aStart 0 aLen
  where
    aStartPtr = unsafeForeignPtrToPtr aStart
    aLen = aEnd `minusPtr` aStartPtr

toArray :: ByteString -> Array Word8
toArray (PS fp off len) = Array nfp endPtr endPtr
  where
    nfp = fp `plusForeignPtr` off
    endPtr = unsafeForeignPtrToPtr fp `plusPtr` len

