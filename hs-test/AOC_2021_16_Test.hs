module AOC_2021_16_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2021_16 (Content (..), Packet (..), readPacket)

unit_2021_16_hex1 :: IO ()
unit_2021_16_hex1 = readPacket "D2FE28" @?= Packet 6 (Literal 2021)

unit_2021_16_hex2 :: IO ()
unit_2021_16_hex2 = readPacket "38006F45291200" @?= Packet 1 (Operator 6 [Packet 0 (Literal 10), Packet 0 (Literal 20)])
