{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash, UnboxedTuples #-}

module Data.Word.Word24.Internal
    ( Word24(..)
    , byteSwap24
    ) where

import Data.X24Bit.Internal
import GHC.Word

import Data.Bits
import Data.Maybe

import GHC.Base
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Read
import GHC.Arr
import GHC.Show

------------------------------------------------------------------------
-- type Word24
------------------------------------------------------------------------

-- Word24 is represented in the same way as Word. Operations may assume
-- and must ensure that it holds only values from its logical range.

data Word24 = W24# Word# deriving (Eq, Ord)
-- ^ 24-bit unsigned integer type

instance Show Word24 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Num Word24 where
    (W24# x#) + (W24# y#)  = W24# (narrow24Word# (x# `plusWord#` y#))
    (W24# x#) - (W24# y#)  = W24# (narrow24Word# (x# `minusWord#` y#))
    (W24# x#) * (W24# y#)  = W24# (narrow24Word# (x# `timesWord#` y#))
    negate (W24# x#)       = W24# (narrow24Word# (int2Word# (negateInt# (word2Int# x#))))
    abs x                  = x
    signum 0               = 0
    signum _               = 1
    fromInteger i          = W24# (narrow24Word# (integerToWord i))

instance Real Word24 where
    toRational x = toInteger x % 1

instance Enum Word24 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Word24"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Word24"
    toEnum i@(I# i#)
        | i >= 0 && i <= fromIntegral (maxBound::Word24)
                        = W24# (int2Word# i#)
        | otherwise     = toEnumError "Word24" i (minBound::Word24, maxBound::Word24)
    fromEnum (W24# x#)  = I# (word2Int# x#)
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral Word24 where
    quot    (W24# x#) y@(W24# y#)
        | y /= 0                    = W24# (x# `quotWord#` y#)
        | otherwise                 = divZeroError
    rem     (W24# x#) y@(W24# y#)
        | y /= 0                    = W24# (x# `remWord#` y#)
        | otherwise                 = divZeroError
    div     (W24# x#) y@(W24# y#)
        | y /= 0                    = W24# (x# `quotWord#` y#)
        | otherwise                 = divZeroError
    mod     (W24# x#) y@(W24# y#)
        | y /= 0                    = W24# (x# `remWord#` y#)
        | otherwise                 = divZeroError
    quotRem (W24# x#) y@(W24# y#)
        | y /= 0                  = case x# `quotRemWord#` y# of
                                    (# q, r #) ->
                                        (W24# q, W24# r)
        | otherwise                 = divZeroError
    divMod  (W24# x#) y@(W24# y#)
        | y /= 0                    = (W24# (x# `quotWord#` y#), W24# (x# `remWord#` y#))
        | otherwise                 = divZeroError
    toInteger (W24# x#)             = smallInteger (word2Int# x#)

instance Bounded Word24 where
    minBound = 0
    maxBound = 0xFFFF

instance Ix Word24 where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral (i - m)
    inRange (m,n) i     = m <= i && i <= n

instance Read Word24 where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Word24 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (W24# x#) .&.   (W24# y#)  = W24# (x# `and#` y#)
    (W24# x#) .|.   (W24# y#)  = W24# (x# `or#`  y#)
    (W24# x#) `xor` (W24# y#)  = W24# (x# `xor#` y#)
    complement (W24# x#)       = W24# (x# `xor#` mb#)
        where !(W24# mb#) = maxBound
    (W24# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)  = W24# (narrow24Word# (x# `shiftL#` i#))
        | otherwise            = W24# (x# `shiftRL#` negateInt# i#)
    (W24# x#) `shiftL` (I# i#)       = W24# (narrow24Word# (x# `shiftL#` i#))
    (W24# x#) `unsafeShiftL` (I# i#) =
        W24# (narrow24Word# (x# `uncheckedShiftL#` i#))
    (W24# x#) `shiftR`       (I# i#) = W24# (x# `shiftRL#` i#)
    (W24# x#) `unsafeShiftR` (I# i#) = W24# (x# `uncheckedShiftRL#` i#)
    (W24# x#) `rotate`       (I# i#)
        | isTrue# (i'# ==# 0#) = W24# x#
        | otherwise  = W24# (narrow24Word# ((x# `uncheckedShiftL#` i'#) `or#`
                                            (x# `uncheckedShiftRL#` (24# -# i'#))))
        where
        !i'# = word2Int# (int2Word# i# `and#` 15##)
    bitSizeMaybe i            = Just (finiteBitSize i)
    bitSize i                 = finiteBitSize i
    isSigned _                = False
    popCount (W24# x#)        = I# (word2Int# (popCnt24# x#))
    bit                       = bitDefault
    testBit                   = testBitDefault

instance FiniteBits Word24 where
    finiteBitSize _ = 24
    countLeadingZeros  (W24# x#) = I# (word2Int# (clz24# x#))
    countTrailingZeros (W24# x#) = I# (word2Int# (ctz24# x#))

-- | Swap bytes in 'Word24'.
byteSwap24 :: Word24 -> Word24
byteSwap24 (W24# w#) = W24# (narrow24Word# (byteSwap24# w#))

{-# RULES
"fromIntegral/Word8->Word24"   fromIntegral = \(W8# x#) -> W24# x#
"fromIntegral/Word24->Word24"  fromIntegral = id :: Word24 -> Word24
"fromIntegral/Word24->Integer" fromIntegral = toInteger :: Word24 -> Integer
"fromIntegral/a->Word24"       fromIntegral = \x -> case fromIntegral x of W# x# -> W24# (narrow24Word# x#)
"fromIntegral/Word24->a"       fromIntegral = \(W24# x#) -> fromIntegral (W# x#)
  #-}

{-# RULES
"properFraction/Float->(Word24,Float)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Word24) n, y :: Float) }
"truncate/Float->Word24"
    truncate = (fromIntegral :: Int -> Word24) . (truncate :: Float -> Int)
"floor/Float->Word24"
    floor    = (fromIntegral :: Int -> Word24) . (floor :: Float -> Int)
"ceiling/Float->Word24"
    ceiling  = (fromIntegral :: Int -> Word24) . (ceiling :: Float -> Int)
"round/Float->Word24"
    round    = (fromIntegral :: Int -> Word24) . (round  :: Float -> Int)
  #-}

{-# RULES
"properFraction/Double->(Word24,Double)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Word24) n, y :: Double) }
"truncate/Double->Word24"
    truncate = (fromIntegral :: Int -> Word24) . (truncate :: Double -> Int)
"floor/Double->Word24"
    floor    = (fromIntegral :: Int -> Word24) . (floor :: Double -> Int)
"ceiling/Double->Word24"
    ceiling  = (fromIntegral :: Int -> Word24) . (ceiling :: Double -> Int)
"round/Double->Word24"
    round    = (fromIntegral :: Int -> Word24) . (round  :: Double -> Int)
  #-}
