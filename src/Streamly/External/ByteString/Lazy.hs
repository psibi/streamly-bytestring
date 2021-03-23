{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Streamly.External.ByteString.Lazy
  ( readChunks
  , read

  , toChunks
  , fromChunks
  , fromChunksIO
  )
where

import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.Word (Word8)
import Streamly.Internal.Data.Array.Foreign.Type (Array)
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Prelude (SerialT)
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.ByteString.Lazy.Internal as BSLI
import qualified Streamly.External.ByteString as Strict
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Prelude as S

import Prelude hiding (read)

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
read = Unfold.many readChunks Array.read

-- | Convert a lazy 'ByteString' to a serial stream of 'Array' 'Word8'.
{-# INLINE toChunks #-}
toChunks :: Monad m => ByteString -> SerialT m (Array Word8)
toChunks = S.unfold readChunks

{-
newtype LazyIO a = LazyIO { runLazy :: IO a } deriving (Functor, Applicative)

liftToLazy :: IO a -> LazyIO a
liftToLazy = LazyIO

instance Monad LazyIO where
    return = pure
    LazyIO a >>= f = LazyIO (unsafeInterleaveIO a >>= unsafeInterleaveIO . runLazy . f)
-}

-- | Convert a serial stream of 'Array' 'Word8' to a lazy 'ByteString'.
--
-- IMPORTANT NOTE: This function is lazy only for lazy monads
-- (e.g. Identity). For strict monads (e.g. /IO/) it consumes the entire input
-- before generating the output. For /IO/ monad please use fromChunksIO
-- instead.
--
-- For strict monads like /IO/ you could create a newtype wrapper to make the
-- monad bind operation lazy and lift the stream to that type using hoist, then
-- you can use this function to generate the bytestring lazily. For example you
-- can wrap the /IO/ type to make the bind lazy like this:
--
-- @
-- newtype LazyIO a = LazyIO { runLazy :: IO a } deriving (Functor, Applicative)
--
-- liftToLazy :: IO a -> LazyIO a
-- liftToLazy = LazyIO
--
-- instance Monad LazyIO where
--   return = pure
--   LazyIO a >>= f = LazyIO (unsafeInterleaveIO a >>= unsafeInterleaveIO . runLazy . f)
-- @
--
-- /fromChunks/ can then be used as,
-- @
-- {-# INLINE fromChunksIO #-}
-- fromChunksIO :: SerialT IO (Array Word8) -> IO ByteString
-- fromChunksIO str = runLazy (fromChunks (S.hoist liftToLazy str))
-- @
{-# INLINE fromChunks #-}
fromChunks :: Monad m => SerialT m (Array Word8) -> m ByteString
fromChunks = S.foldr BSLI.chunk Empty . S.map Strict.fromArray

-- | Convert a serial stream of 'Array' 'Word8' to a lazy 'ByteString' in the
-- /IO/ monad.
{-# INLINE fromChunksIO #-}
fromChunksIO :: SerialT IO (Array Word8) -> IO ByteString
fromChunksIO =
-- Although the /IO/ monad is strict in nature we emulate laziness using
-- 'unsafeInterleaveIO'.
    S.foldrM
        (\x b -> unsafeInterleaveIO b >>= pure . BSLI.chunk x)
        (pure BSLI.Empty) .
    S.map Strict.fromArray
