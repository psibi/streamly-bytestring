{-# LANGUAGE CPP #-}

module Streamly.External.ByteString.Lazy
  (read, write, write')
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.Word (Word8)
import Data.List (foldl')
import Streamly.Internal.Memory.Array.Types (Array(..))
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
import Streamly.Internal.Data.Unfold (concat)
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Data.Fold.Types (Fold(..))

import qualified Streamly.External.ByteString as Strict
import qualified Streamly.Internal.Memory.Array.Types as A

import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable(..))

import Prelude hiding (concat, read)

-- One can convert a serial stream of 'Array' 'Word8' to a lazy 'ByteString'.
-- It's OK to use foldr in this case, as, in general the number of
-- chunks will not be extremely high.
-- @
-- {-# INLINE fromArrayStream #-}
-- fromArrayStream :: Monad m => SerialT m (Array Word8) -> m ByteString
-- fromArrayStream = S.foldr chunk Empty . S.map Strict.fromArray
-- @

-- One can convert a lazy 'ByteString' to a serial stream of 'Array' 'Word8'.
-- @
-- {-# INLINE toArrayStream #-}
-- toArrayStream :: Monad m => ByteString -> SerialT m (Array Word8)
-- toArrayStream = S.unfold unf
--   where
--     unf = Unfold step seed
--     seed = return
--     step (Chunk bs bl) = return $ Yield (Strict.toArray bs) bl
--     step Empty = return $ Stop
-- @

-- Default chunk size in number of bytes
{-# INLINE defaultChunkSize #-}
defaultChunkSize :: Int
defaultChunkSize = 32000

-- Fold a stream of Word8 to 32k sized chunks of lazy ByteString
{-# INLINE write #-}
write :: MonadIO m => Fold m Word8 ByteString
write = write' defaultChunkSize

-- Fold a stream of Word8 to 'chunkSize' sized chunks of lazy ByteString
{-# INLINE write' #-}
write' :: MonadIO m => Int -> Fold m Word8 ByteString
write' chunkSize = Fold step initial extract
  where
    insertElem (Array start end bound) x = do
      liftIO $ poke end x
      return $ Array start (end `plusPtr` sizeOf (undefined :: Word8)) bound

    initial = do
      newArr <- liftIO $ A.newArray chunkSize
      return [newArr :: Array Word8]

    step arrList@(Array _ end bound:_) x
      | end == bound = do
          newArr <- liftIO $ A.newArray chunkSize
          newArr' <- insertElem newArr x
          return $ newArr':arrList
    step (arr:arrTail) x = do
      arr' <- insertElem arr x
      return $ arr':arrTail
    step _ _ = undefined

    extract (arr:arrTail) = do
      arr' <- liftIO $ A.shrinkToFit arr
      return $ foldl' (\b a -> Chunk (Strict.fromArray a) b) Empty $ arr':arrTail
    extract _ = undefined

-- Unfold a lazy Bytestring to a stream of Word8
{-# INLINE read #-}
read :: Monad m => Unfold m ByteString Word8
read = concat (Unfold step inject) Strict.read
  where
    inject = return
    step (Chunk h t) = return $ Yield h t
    step Empty = return $ Stop





