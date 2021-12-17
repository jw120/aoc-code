module AOC_2021_16_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2021_16 (Content (..), Packet (..), readPacket)

unit_2021_16_hex1 :: IO ()
unit_2021_16_hex1 =
    readPacket "D2FE28"
        @?= Packet 6 (Literal 2021)

unit_2021_16_hex2 :: IO ()
unit_2021_16_hex2 =
    readPacket "38006F45291200"
        @?= Packet 1 (Operator 6 [Packet 0 (Literal 10), Packet 0 (Literal 20)])

-- 111  011  1  00000000011  010 100 00001  100 100 00010  001 100 00011  00000
-- VVV  TTT  I  LLLLLLLLLLL  AAA AAA AAAAA  BBB BBB BBBBB  CCC CCC CCCCC
-- V7   Op3    3 packets     V2  Lit     1  V4  Lit     2  V1  Lit 3

unit_2021_16_hex3 :: IO ()
unit_2021_16_hex3 =
    readPacket "EE00D40C823060"
        @?= Packet 7 (Operator 3 [Packet 2 (Literal 1), Packet 4 (Literal 2), Packet 1 (Literal 3)])
