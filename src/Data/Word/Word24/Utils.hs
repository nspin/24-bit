module Data.Word.Word24.Utils
    ( fromOctets
    , toOctets
    ) where

import Data.Word.Word24
import Data.Bits

fromOctets :: Word8 -> Word8 -> Word8 -> Word24
fromOctets a b c = a .|. shift 8 b .|. shift 16 c

toOctets :: Word24 -> (Word8, Word8, Word8)
toOctets w = ( fromIntegral w
             , fromIntegral (shift -8 w)
             , fromIntegral (shift -16 w)
             )
