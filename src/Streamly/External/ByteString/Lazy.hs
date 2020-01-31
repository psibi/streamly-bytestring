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
import Streamly.Internal.Memory.Array.Types (Array(..))
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
import Streamly.Internal.Data.Unfold (concat)
import Streamly.Internal.Data.Unfold.Types (Unfold(..))

import qualified Streamly.External.ByteString as Strict
import qualified Streamly.Internal.Memory.Array as A

import System.IO.Unsafe (unsafeInterleaveIO)

import Streamly
import qualified Streamly.Internal.Prelude as S

import qualified Data.ByteString.Lazy.Internal as BSLI

import Prelude hiding (concat, read)

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
read = concat readChunks A.read

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
-- As a consequence of using 'S.foldr' in the implementation the effects of
-- the stream are evaluated if the monad is strict. And hence, since IO is a
-- strict monad, this function does not work as expected when interacting with
-- the file API's (Eg. Functions from streamly:Streamly.FileSystem.Handle).
--
-- In such cases we need to use a lazy version of IO.
-- One can define a lazy version of IO like so,
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
--
-- We can compose /liftToLazy/ with /liftLazyToM/ (/liftLazyToM . liftToLazy/) to
-- lift the evaluation to the desired monad. Please note that the desired monad
-- here should be lazy in nature.
{-# INLINE fromChunks #-}
fromChunks :: Monad m => SerialT m (Array Word8) -> m ByteString
fromChunks = S.foldr BSLI.chunk Empty . S.map Strict.fromArray

-- | Convert a serial stream of 'Array' 'Word8' to a lazy 'ByteString' in the /IO/
-- monad.
{-# INLINE fromChunksIO #-}
fromChunksIO :: SerialT IO (Array Word8) -> IO ByteString
fromChunksIO =
    S.foldrM (\x b -> unsafeInterleaveIO b >>= pure . BSLI.chunk x) (pure BSLI.Empty)
    . S.map Strict.fromArray
