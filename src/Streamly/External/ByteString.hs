{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

module Streamly.External.ByteString
  ( toArray
  , fromArray

  , reader

  , writeN
  , write

  -- Deprecated
  , read
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
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Internal.Data.Unfold as Unfold (fold, mkUnfoldrM)

#if MIN_VERSION_streamly_core(0,2,0)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutByteArray (MutByteArray(..))
import qualified Streamly.Internal.Data.MutByteArray as MutBA (nil)
import qualified Streamly.Internal.Data.Stream as StreamD (Step(Yield))
#else
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Unboxed (MutableByteArray(..))
import qualified Streamly.Internal.Data.Unboxed as Unboxed (nil)
import qualified Streamly.Internal.Data.Stream.StreamD as StreamD (Step(Yield))
#endif

import Prelude hiding (read)

{-# INLINE mutableByteArrayContents# #-}
mutableByteArrayContents# :: MutableByteArray# RealWorld -> Addr#
mutableByteArrayContents# marr# = byteArrayContents# (unsafeCoerce# marr#)

-- | Helper function that creates a ForeignPtr
{-# INLINE makeForeignPtr #-}
makeForeignPtr :: MutByteArray -> Int -> ForeignPtr a
makeForeignPtr (MutByteArray marr#) (I# off#) =
    ForeignPtr
        (mutableByteArrayContents# marr# `plusAddr#` off#)
        (PlainPtr marr#)

-- | Convert a 'ByteString' to an array of 'Word8'. It can be done in constant
-- time only for GHC allocated memory. For foreign allocator allocated memory
-- there is a copy involved.
{-# INLINE toArray #-}
toArray :: ByteString -> Array Word8
toArray (BS (ForeignPtr addr# _) _)
    | Ptr addr# == nullPtr = Array MutBA.nil 0 0
toArray (BS (ForeignPtr addr# (PlainPtr marr#)) len) =
    let off = I# (addr# `minusAddr#` mutableByteArrayContents# marr#)
     in Array (MutByteArray marr#) off (off + len)
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
fromArray (Array {..})
    | aLen == 0 = mempty
    | otherwise = BS (makeForeignPtr arrContents arrStart) aLen

    where

    aLen = arrEnd - arrStart

-- | Unfold a strict ByteString to a stream of Word8.
{-# INLINE reader #-}
reader :: Monad m => Unfold m ByteString Word8
reader = lmap toArray Array.reader

-- | Fold a stream of Word8 to a strict ByteString of given size in bytes.
{-# INLINE writeN #-}
writeN :: MonadIO m => Int -> Fold m Word8 ByteString
writeN i = fromArray <$> Array.writeN i

-- | Fold a stream of Word8 to a strict ByteString of appropriate size.
{-# INLINE write #-}
write :: MonadIO m => Fold m Word8 ByteString
write = fromArray <$> Array.write

--------------------------------------------------------------------------------
-- Deprecated
--------------------------------------------------------------------------------

{-# DEPRECATED read "Please use reader instead." #-}
{-# INLINE read #-}
read :: Monad m => Unfold m ByteString Word8
read = reader
