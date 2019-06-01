{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.ByteString where

import Control.Monad (liftM2)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString hiding (length)
import Data.ByteString.Unsafe
import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr (minusPtr)
import Prelude hiding (length)
import Streamly
import Streamly.Mem.Array
import qualified Streamly.Prelude as S

toByteString ::
     forall m. (MonadIO m, MonadAsync m)
  => SerialT m (Array Word8)
  -> m ByteString
toByteString stream =
  let xs = S.mapM arrayToByteString stream
      ys = S.foldlM' (\a b -> pure $ a <> b) mempty xs
   in ys

arrayToByteString :: (MonadIO m) => Array Word8 -> m ByteString
arrayToByteString arr
  | length arr == 0 = return mempty
arrayToByteString Array {..} =
  liftIO $
  withForeignPtr aStart $ \ptr ->
    unsafePackCStringFinalizer ptr aLen (return ())
  where
    aLen =
      let p = unsafeForeignPtrToPtr aStart
       in aEnd `minusPtr` p
