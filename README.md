# aeson-utils

[![Build Status](https://travis-ci.org/silkapp/aeson-utils.svg?branch=master)](https://travis-ci.org/silkapp/aeson-utils)

A small package containing helpful addititions to aeson.

It provides `eitherDecodeV` and `decodeV` functions (analogous to `eitherDecode` and `decode` in aeson) that can be used to parse atomic JSON values at the top level. This is not strictly valid JSON, only objects and arrays are allowed at the top level in the specification, but aeson's `encode` allows this already. This gives us the nice property `decodeV . encode = Just`.

The package also contains the `.=?` operator that you can use to optionally construct object pairs:

```haskell
object
  [ "a" .: foo
  , "b" .: bar
  ] ++ catMaybes
  [ "c" .:? mBaz
  , "d" .:? mQux
  ]
```

Finally it has a `parseNumber` function that converts a scientific into `Either Integer Double` in case you want to distinguish between integrals and fractionals.

## Install instructions

aeson-utils is available on hackage

```
cabal install aeson-utils
```
