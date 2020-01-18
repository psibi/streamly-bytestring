{-# LANGUAGE CPP #-}

module Streamly.External.ByteString.Lazy
  (fromArrayStream, toArrayStream)
where

import Data.ByteString.Lazy.Internal (ByteString(..), chunk)
import Data.Word (Word8)
import Streamly.External.ByteString as Strict
import Streamly.Internal.Memory.Array.Types (Array(..))
import Streamly
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
import Streamly.Internal.Data.Unfold.Types (Unfold(..))

import qualified Streamly.Prelude as S

-- Convert a serial stream of 'Array' 'Word8' to a lazy 'ByteString'.
-- It's OK to use foldr in this case, as, in general the number of
-- chunks will not be extremely high.
{-# INLINE fromArrayStream #-}
fromArrayStream :: Monad m => SerialT m (Array Word8) -> m ByteString
fromArrayStream = S.foldr chunk Empty . S.map Strict.fromArray

-- Convert a lazy 'ByteString' to a serial stream of 'Array' 'Word8'.
{-# INLINE toArrayStream #-}
toArrayStream :: Monad m => ByteString -> SerialT m (Array Word8)
toArrayStream = S.unfold unf
  where
    unf = Unfold step seed
    seed = return
    step (Chunk bs bl) = return $ Yield (Strict.toArray bs) bl
    step Empty = return $ Stop

