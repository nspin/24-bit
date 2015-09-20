module Data.Word.Word24.Utils
    ( fromOctets
    , toOctets
    ) where

import Data.Bits
import Data.Word
import Data.Word.Word24

fromOctets :: Word8 -> Word8 -> Word8 -> Word24
fromOctets a b c =  fromIntegral a
                .|. (fromIntegral b `shiftR` 8)
                .|. (fromIntegral c `shiftR` 16)

toOctets :: Word24 -> (Word8, Word8, Word8)
toOctets w = ( fromIntegral w
             , fromIntegral (w `shiftL` 8)
             , fromIntegral (w `shiftL` 16)
             )
