{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

module Streamly.External.ByteString
  ( toArray
  , fromArray

  , read
  , writeN
  , write
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peek)
import GHC.Exts
    ( Addr#
    , MutableByteArray#
    , RealWorld
    , byteArrayContents#
    , minusAddr#
    , plusAddr#
    , unsafeCoerce#
    )
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(..))
import GHC.Int (Int(..))
import GHC.Ptr (Ptr(..), nullPtr, plusPtr)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Unfold (Unfold, lmap)

-- Internal imports
import Data.ByteString.Internal (ByteString(..))
import Streamly.Internal.Data.Array.Unboxed.Mut.Type (nilArrayContents)
import Streamly.Internal.Data.Array.Unboxed.Type (Array(..))
import Streamly.Data.Unbox (MutableByteArray(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Data.Array.Unboxed as Array
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Stream.StreamD as StreamD

import Prelude hiding (read)

{-# INLINE mutableByteArrayContents# #-}
mutableByteArrayContents# :: MutableByteArray# RealWorld -> Addr#
mutableByteArrayContents# marr# = byteArrayContents# (unsafeCoerce# marr#)

-- | Helper function that creates a ForeignPtr
{-# INLINE makeForeignPtr #-}
makeForeignPtr :: MutableByteArray a -> Int -> ForeignPtr a
makeForeignPtr (MutableByteArray marr#) (I# off#) =
    ForeignPtr
        (mutableByteArrayContents# marr# `plusAddr#` off#)
        (PlainPtr marr#)

-- | Convert a 'ByteString' to an array of 'Word8'. This function unwraps the
-- 'ByteString' and wraps it with 'Array' constructors and hence the operation
-- is performed in constant time.
{-# INLINE toArray #-}
toArray :: ByteString -> Array Word8
toArray (BS (ForeignPtr addr# _) _)
    | Ptr addr# == nullPtr = Array nilArrayContents 0 0
toArray (BS (ForeignPtr addr# (PlainPtr marr#)) len) =
    let off = I# (addr# `minusAddr#` mutableByteArrayContents# marr#)
     in Array (MutableByteArray marr#) off (off + len)
toArray (BS fptr len) =
    unsafeInlineIO
        $ withForeignPtr fptr $ Unfold.fold (Array.writeN len) generator

    where

    generator =
        Unfold.mkUnfoldrM
            (\ptr -> flip StreamD.Yield (ptr `plusPtr` 1) <$> peek ptr)

-- | Convert an array of 'Word8' to a 'ByteString'. This function unwraps the
-- 'Array' and wraps it with 'ByteString' constructors and hence the operation
-- is performed in constant time.
{-# INLINE fromArray #-}
fromArray :: Array Word8 -> ByteString
fromArray Array {..}
    | aLen == 0 = mempty
    | otherwise = BS (makeForeignPtr arrContents arrStart) aLen
  where
    aLen = arrEnd - arrStart

-- | Unfold a strict ByteString to a stream of Word8.
{-# INLINE read #-}
read :: Monad m => Unfold m ByteString Word8
read = lmap toArray Array.read

-- | Fold a stream of Word8 to a strict ByteString of given size in bytes.
{-# INLINE writeN #-}
writeN :: MonadIO m => Int -> Fold m Word8 ByteString
writeN i = fromArray <$> Array.writeN i

-- | Fold a stream of Word8 to a strict ByteString of appropriate size.
{-# INLINE write #-}
write :: MonadIO m => Fold m Word8 ByteString
write = fromArray <$> Array.write
