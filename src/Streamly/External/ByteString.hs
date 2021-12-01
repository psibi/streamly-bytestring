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
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Ptr (Ptr(..), minusPtr, nullPtr, plusPtr)
import Streamly.Data.Unfold (Unfold, lmap)
import Streamly.Data.Fold (Fold)

-- Internal imports
import Data.ByteString.Internal (ByteString(..))
import Streamly.Internal.Data.Array.Foreign.Type (Array(..))
import Streamly.Internal.Data.Array.Foreign.Mut.Type
    (ArrayContents, arrayToFptrContents, fptrToArrayContents, nilArrayContents)

import qualified Streamly.Data.Array.Foreign as A

import Prelude hiding (read)

-- | Helper function that creates a ForeignPtr
makeForeignPtr :: ArrayContents -> Ptr a -> ForeignPtr a
makeForeignPtr contents (Ptr addr#) =
    ForeignPtr addr# (arrayToFptrContents contents)

-- | Convert a 'ByteString' to an array of 'Word8'. This function unwraps the
-- 'ByteString' and wraps it with 'Array' constructors and hence the operation
-- is performed in constant time.
{-# INLINE toArray #-}
toArray :: ByteString -> Array Word8
toArray (PS (ForeignPtr addr# _) _ _)
    | Ptr addr# == nullPtr = Array nilArrayContents nullPtr nullPtr
toArray (PS (ForeignPtr addr# fpcontents) off len) =
    Array (fptrToArrayContents fpcontents) startPtr endPtr
  where
    startPtr = Ptr addr# `plusPtr` off
    endPtr = startPtr `plusPtr` len

-- | Convert an array of 'Word8' to a 'ByteString'. This function unwraps the
-- 'Array' and wraps it with 'ByteString' constructors and hence the operation
-- is performed in constant time.
{-# INLINE fromArray #-}
fromArray :: Array Word8 -> ByteString
fromArray Array {..}
    | aLen == 0 = mempty
    | otherwise = PS (makeForeignPtr arrContents arrStart) 0 aLen
  where
    aLen = aEnd `minusPtr` arrStart

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
