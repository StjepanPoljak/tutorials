module Main where

import Data.Char (intToDigit)
import Numeric (showHex, showIntAtBase)
import Data.Bits

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
