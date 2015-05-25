# Changelog

#### 0.3.0.2

* Allow `aeson 0.9.*`. Because of the aeson release `eitherDecodeV` and `decodeV` now behave the same as `eitherDecode` and `decode` respectively, this package can still be used as a compatibility layer.

#### 0.3.0.1

* Allow `attoparsec 0.13.*`

## 0.3

* The semantics of `eitherDecodeV` and `decodeV` have been changed to conform to aeson's `eitherDecode` and `decode`: They now consume until end of input and strip trailing whitespace.

  This is necessary to remove ambiguity when decoding JSON-like values that are
  not necessarily properly formatted for JSON. For example, parsing a hexadecimal
  digit sequence that starts with digits but does not have double quotes should
  fail to parse rather than be interpreted as a number.

#### 0.2.2.1

* Allow `attoparsec 0.12.*`

### 0.2.2.0

* Deprecate `parseNumber` in favor of `Data.Scientific.floatingOrInteger`
* Re-export `Data.Scientific.floatingOrInteger`

#### 0.2.1.1

* Allow `scientific 0.3.*`
