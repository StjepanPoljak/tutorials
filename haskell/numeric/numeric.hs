module Main where

import Data.Char (intToDigit)
import Numeric (showHex, showIntAtBase)
import Data.Bits
import Data.Ratio

-- Double       double-precision floating point                             --
-- Float        single-precision floating point                             --
-- Int          fixed-precision signed integer (min. range [-2^29..2^29-1]) --
-- Integer      arbitrary-precision signed integer (unlimited range)        --
-- Rational     arbitrary-precision rational numbers (ratio of Integers)    --
-- Word         fixed-precision unsigned integer (storage size same as Int) --
--
-- | bits | signed | unsigned |                                             --
-- |------|--------|----------|                                             --
-- |  8   |  Int8  |  Word8   |                                             --
-- |  16  | Int16  |  Word16  |                                             --
-- |  32  | Int32  |  Word32  |                                             --
-- |  64  | Int64  |  Word64  |                                             --

-- Bits: Integer, Int[16,32,64], Word[16,32,64]                             --
-- Bounded: Int[16,32,64], Word[16,32,64]                                   --
-- Floating: Double, Float                                                  --
-- Fractional: Double, Float, Rational                                      --
-- Integral: Integer, Int[16,32,64], Word[16,32,64]                         --
-- Num: Double, Float, Integer, Int[16,32,64], Word[16,32,64], Rational     --
-- Real: Double, Float, Integer, Int[16,32,64], Word[16,32,64], Rational    --
-- RealFrac: Double, Float, Rational                                        --

-- fromIntegral :: (Integral a, Num b) => a -> b                            --
-- fromRational :: Fractional a => Rational -> a                            --
-- toRational :: Real a => a -> Rational                                    --
-- fromInteger Num a => Integer -> a                                        --
-- toInteger Integral a => a -> Integer                                     --
-- truncate :: (RealFrac a, Integral b) => a -> b                           --
-- round :: (RealFrac a, Integral b) => a -> b                              --
-- floor :: (RealFrac a, Integral b) => a -> b                              --
-- ceiling :: (RealFrac a, Integral b) => a -> b                            --

-- (**) :: Floating a => a -> a -> a                                        --
-- (^) :: (Num a, Integral b) => a -> b -> a                                --
-- (^^) :: (Fractional a, Integral b) => a -> b -> a                        --
-- (%) :: Integral a => a -> a -> Ratio a                                   --

main = do
    putStrLn $ showHex 15 ""

--                         base         number      --
--                           |            ||        --
    putStrLn $ showIntAtBase 8 intToDigit 17 ""
--                             |        |           --
--                      function for converting     --
--                        digits to characters      --

    putStrLn $ showHex (0xef .|. 0x10 :: Int) ""
    putStrLn $ showHex (0xef .&. 0x10 :: Int) ""
    putStrLn $ showHex (0xef `xor` 0x10 :: Int) ""

    putStrLn $ showHex (0x02 `shift` 1 :: Int) ""
    putStrLn $ showHex (0x02 `shift` (-1) :: Int) ""

    putStrLn $ show $ 5.3 ** 2.5
    putStrLn $ show $ 5.3 ^ 2
    putStrLn $ show $ 5 % 2
    putStrLn $ show $ (5 % 2) ^^ 3

