# streamly-bytestring

Library for streamly and bytestring interoperation.

`Streamly.Memory.Array` is a generalization of `ByteString`.
`Streamly.Memory.Array` used proper monadic programming instead
of `unsafePerformIO`.

This library is for interoperation with legacy programs that
already use `ByteString`. In case you are writing code from scratch,
consider using `Streamly.Memory.Array`.

## Description

This package provides the following modules:

1. `Streamly.External.ByteString`
2. `Streamly.External.ByteString.Lazy`

### Strict ByteString

`Streamly.External.ByteString` provides functions to for
interoperation between streamly and strict bytestring.

#### Direct conversions

1. `fromArray`
2. `toArray`

These functions are used for efficient _O(1)_ conversion between
streamly's pinned array type (`Streamly.Memory.Array`) and `ByteString`.

#### Unfolds & Folds

1. `read`
2. `writeN`
3. `write`

These functions can be used with `unfold` and `fold` available in
`Streamly.Prelude` to produce and consume streams.

`writeN` is more efficient than `write` and should be preferred over
`write` when the size of the stream is known.

### Lazy Bytestring

`Streamly.External.ByteString.Lazy` provides functions to for
interoperation between streamly and lazy bytestring.

#### Unfolds & Folds

1. `readChunks`
2. `read`

These functions can be used with `unfold` available in
`Streamly.Prelude` to produce stream of `Array Word8` or `Word8`.

#### To/From streams

1. `toChunks` = `unfold readChunks`
2. `fromChunks`
3. `fromChunksIO`

`fromChunksIO` is similar to `fromChunks` but makes sure that the
resulting `ByteString` is lazy in nature.

As a consequence of using `foldr` in the implementation the effects of
the stream are evaluated if the monad is strict. And hence, since IO is a
strict monad, this function does not work as expected when interacting with
the file API's (Eg. Functions from `streamly:Streamly.FileSystem.Handle`).

In such cases we need to use a lazy version of IO.

One can define a lazy version of IO like so,

```
newtype LazyIO a = LazyIO { runLazy :: IO a } deriving (Functor, Applicative)

liftToLazy :: IO a -> LazyIO a
liftToLazy = LazyIO

instance Monad LazyIO where
  return = pure
  LazyIO a >>= f = LazyIO (unsafeInterleaveIO a >>= unsafeInterleaveIO . runLazy . f)
```

`fromChunks` can then be used as,

```
{-# INLINE fromChunksIO #-}
fromChunksIO :: SerialT IO (Array Word8) -> IO ByteString
fromChunksIO str = runLazy (fromChunks (S.hoist liftToLazy str))
```


We can compose `liftToLazy` with `liftLazyToM` (`liftLazyToM . liftToLazy`) to
lift the evaluation to the desired monad. Please note that the desired monad
here should be lazy in nature.

See [this issue](https://github.com/psibi/streamly-bytestring/issues/7) for more details.

## Usage

This is a dumb program that counts the number of bytes in a file.

```
import Streamly
import qualified Streamly.Prelude as S

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Streamly.External.ByteString as Strict
import qualified Streamly.External.ByteString.Lazy as Lazy

import System.IO (FilePath)

strictByteStringSize :: BS.ByteString -> IO Int
strictByteStringSize bs = S.length $ S.unfold Strict.read bs

lazyByteStringSize :: BSL.ByteString -> IO Int
lazyByteStringSize bsl = S.foldl' (+) 0
                         $ S.mapM strictByteStringSize
                         $ S.map Strict.fromArray
                         $ Lazy.toChunks bsl

fileSize :: FilePath -> IO Int
fileSize path = do
    bsl <- BSL.readFile path
    lazyByteStringSize bsl
```










