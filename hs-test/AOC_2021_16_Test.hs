module AOC_2021_16_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2021_16 (Content (..), Packet (..), eval, readPacket, versionSum)

-- D2FE28
--110 100 10111 11110 00101 000
--VVV TTT AAAAA BBBBB CCCCC
--V6  Lit  (7*256 + 14*16 + 5 = 2021)

unit_2021_16_hex1 :: IO ()
unit_2021_16_hex1 =
    readPacket "D2FE28"
        @?= Packet 6 (Literal 2021)

-- 38006F45291200
-- 001  110  0  000000000011011  110 100 01010  010 100 10001 00100 0000000
-- VVV  TTT  I  LLLLLLLLLLLLLLL  AAA AAA AAAAA  BBB BBB BBBBBBBBBB
-- V1   Op6       Length 27      V6  Lit  1  0  V2  Lit     20

unit_2021_16_hex2 :: IO ()
unit_2021_16_hex2 =
    readPacket "38006F45291200"
        @?= Packet 1 (Operator 6 [Packet 6 (Literal 10), Packet 2 (Literal 20)])

-- 111  011  1  00000000011  010 100 00001  100 100 00010  001 100 00011  00000
-- VVV  TTT  I  LLLLLLLLLLL  AAA AAA AAAAA  BBB BBB BBBBB  CCC CCC CCCCC
-- V7   Op3    3 packets     V2  Lit     1  V4  Lit     2  V1  Lit 3

unit_2021_16_hex3 :: IO ()
unit_2021_16_hex3 =
    readPacket "EE00D40C823060"
        @?= Packet 7 (Operator 3 [Packet 2 (Literal 1), Packet 4 (Literal 2), Packet 1 (Literal 3)])

unit_2021_16_version1 :: IO ()
unit_2021_16_version1 = versionSum (readPacket "D2FE28") @?= 6

unit_2021_16_version2 :: IO ()
unit_2021_16_version2 = versionSum (readPacket "38006F45291200") @?= 9

unit_2021_16_version3 :: IO ()
unit_2021_16_version3 = versionSum (readPacket "EE00D40C823060") @?= 14

unit_2021_16_version4 :: IO ()
unit_2021_16_version4 = versionSum (readPacket "8A004A801A8002F478") @?= 16

unit_2021_16_version5 :: IO ()
unit_2021_16_version5 = versionSum (readPacket "620080001611562C8802118E34") @?= 12

unit_2021_16_version6 :: IO ()
unit_2021_16_version6 = versionSum (readPacket "C0015000016115A2E0802F182340") @?= 23

unit_2021_16_version7 :: IO ()
unit_2021_16_version7 = versionSum (readPacket "A0016C880162017C3686B18A3D4780") @?= 31

unit_2021_16_eval1 :: IO ()
unit_2021_16_eval1 = eval (readPacket "C200B40A82") @?= 3

unit_2021_16_eval2 :: IO ()
unit_2021_16_eval2 = eval (readPacket "04005AC33890") @?= 54
unit_2021_16_eval3 :: IO ()
unit_2021_16_eval3 = eval (readPacket "880086C3E88112") @?= 7
unit_2021_16_eval4 :: IO ()
unit_2021_16_eval4 = eval (readPacket "CE00C43D881120") @?= 9
unit_2021_16_eval5 :: IO ()
unit_2021_16_eval5 = eval (readPacket "D8005AC2A8F0") @?= 1
unit_2021_16_eval6 :: IO ()
unit_2021_16_eval6 = eval (readPacket "F600BC2D8F") @?= 0
unit_2021_16_eval7 :: IO ()
unit_2021_16_eval7 = eval (readPacket "9C005AC2F8F0") @?= 0
unit_2021_16_eval8 :: IO ()
unit_2021_16_eval8 = eval (readPacket "9C0141080250320F1802104A08") @?= 1
