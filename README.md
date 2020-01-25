# `streamly-bytestring`

Library for streamly and bytestring interoperation.

## Description

This package provides `Streamly.External.ByteString` and
`Streamly.External.ByteString.Lazy`.

### Strict ByteString

`Streamly.External.ByteString` provides functions to for
interoperation between streamly and strict bytestring.

`fromArray` and `toArray` are used to efficiently convert between
streamly's pinned array type (`Streamly.Memory.Array`) and bytestring.

`read`, `writeN` and `write` are `Unfold`s & `Fold`s provided by streamly
that are used to create and consume a stream of `Word8`. `writeN` is more
efficient than `write` and should be preferred over `write` when possible.

### Lazy Bytestring

`Streamly.External.ByteString.Lazy` provides functions to for
interoperation between streamly and lazy bytestring.

`readChunks` and `read` are `Unfold`s. `unfold` from `Streamly.Prelude` can be
used to create a stream of `Array Word8` or a stream of `Word8` from a
lazy `ByteString`.

`toChunks` is defined as `unfold readChunks`. `fromChunks` can be used to create a
lazy `ByteString` from a stream of `Array Word8` chunks.

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










