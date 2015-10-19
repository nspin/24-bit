# 24-bit

`Word24` and `Int24` types for Haskell.

These types are implemented in EXACTLY the same way as `Word8`, `Word16`, `Word32` (well, depending on your architecture), `Int8`, `Int16`, and `Int32` (again, depending on your architecture) in `base`. That is to say, they wrap actual primitives (types of kind `#`).

This type might be useful when dealing with colors, because RGB values are often specified and passed around as 8-bit triplets, which aren't as fast for aggregate numeric operations.
