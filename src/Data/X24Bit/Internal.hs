{-# LANGUAGE MagicHash #-}

module Data.X24Bit.Internal
    ( byteSwap24#
    , ctz24#
    , clz24#
    , popCnt24#
    , narrow24Int#
    , narrow24Word#
    ) where

import GHC.Base

-- HERE
byteSwap24# :: Word# -> Word#
byteSwap24# w# = let byte0 = uncheckedShiftL#  (and# w# 0xff0000##) 16#
                     byte1 = uncheckedShiftL#  (and# w# 0x00ff00##)  8#
                     byte2 = uncheckedShiftRL# (and# w# 0x0000ff##) 16#
                 in and# byte0 (and# byte1 byte2)

-- HERE
clz24# :: Word# -> Word#
clz24# w# = clz# (or# (not# 0xffffff##) w#)

-- HERE
ctz24# :: Word# -> Word#
ctz24# w# = let x = ctz16# (uncheckedShiftRL# w# 8#)
            in plusWord# x (timesWord# (int2Word# (eqWord# 16## x)) (ctz8# w#))

popCnt24# :: Word# -> Word#
popCnt24# w# = popCnt# (narrow24Word# w#)

-- HERE
narrow24Int# :: Int# -> Int#
narrow24Int# = andI# (word2Int# 0xffffff##)

narrow24Word# :: Word# -> Word#
narrow24Word# = and# 0xffffff##
