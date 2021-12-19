module AOC_2021_18_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2021_18 (add, addList, magnitude, readSnail, reduce, showSnail)

unit_2021_18_snail_reduce1 :: IO ()
unit_2021_18_snail_reduce1 = showSnail (reduce (readSnail "[[[[[9,8],1],2],3],4]")) @?= "[[[[0,9],2],3],4]"

unit_2021_18_snail_reduce2 :: IO ()
unit_2021_18_snail_reduce2 = showSnail (reduce (readSnail "[7,[6,[5,[4,[3,2]]]]]")) @?= "[7,[6,[5,[7,0]]]]"

unit_2021_18_snail_reduce3 :: IO ()
unit_2021_18_snail_reduce3 = showSnail (reduce (readSnail "[[6,[5,[4,[3,2]]]],1]")) @?= "[[6,[5,[7,0]]],3]"

unit_2021_18_snail_reduce4 :: IO ()
unit_2021_18_snail_reduce4 = showSnail (reduce (readSnail "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")) @?= "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"

unit_2021_18_snail_reduce5 :: IO ()
unit_2021_18_snail_reduce5 = showSnail (reduce (readSnail "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")) @?= "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"

unit_2021_18_snail_add1 :: IO ()
unit_2021_18_snail_add1 =
    showSnail (add (readSnail "[[[[4,3],4],4],[7,[[8,4],9]]]") (readSnail "[1,1]")) @?= "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"

unit_2021_18_snail_add_list1 :: IO ()
unit_2021_18_snail_add_list1 =
    let xs = map readSnail ["[1,1]", "[2,2]", "[3,3]", "[4,4]"]
        ans = "[[[[1,1],[2,2]],[3,3]],[4,4]]"
     in showSnail (addList xs) @?= ans

unit_2021_18_snail_add_list2 :: IO ()
unit_2021_18_snail_add_list2 =
    let xs = map readSnail ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]"]
        ans = "[[[[3,0],[5,3]],[4,4]],[5,5]]"
     in showSnail (addList xs) @?= ans

unit_2021_18_snail_add_list3 :: IO ()
unit_2021_18_snail_add_list3 =
    let xs = map readSnail ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]"]
        ans = "[[[[5,0],[7,4]],[5,5]],[6,6]]"
     in showSnail (addList xs) @?= ans

unit_2021_18_snail_add_list4 :: IO ()
unit_2021_18_snail_add_list4 =
    let xs =
            map
                readSnail
                [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
                , "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
                , "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
                , "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
                , "[7,[5,[[3,8],[1,4]]]]"
                , "[[2,[2,2]],[8,[8,1]]]"
                , "[2,9]"
                , "[1,[[[9,3],9],[[9,0],[0,7]]]]"
                , "[[[5,[7,4]],7],1]"
                , "[[[[4,2],2],6],[8,7]]"
                ]
        ans = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
     in showSnail (addList xs) @?= ans

unit_2021_18_magnitude1 :: IO ()
unit_2021_18_magnitude1 = magnitude (readSnail "[9,1]") @?= 29

unit_2021_18_magnitude2 :: IO ()
unit_2021_18_magnitude2 = magnitude (readSnail "[1,9]") @?= 21

unit_2021_18_magnitude3 :: IO ()
unit_2021_18_magnitude3 = magnitude (readSnail "[[9,1],[1,9]]") @?= 129

unit_2021_18_magnitude4 :: IO ()
unit_2021_18_magnitude4 = magnitude (readSnail "[[1,2],[[3,4],5]]") @?= 143

unit_2021_18_magnitude5 :: IO ()
unit_2021_18_magnitude5 = magnitude (readSnail "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]") @?= 1384

unit_2021_18_magnitude6 :: IO ()
unit_2021_18_magnitude6 = magnitude (readSnail "[[[[1,1],[2,2]],[3,3]],[4,4]]") @?= 445

unit_2021_18_magnitude7 :: IO ()
unit_2021_18_magnitude7 = magnitude (readSnail "[[[[3,0],[5,3]],[4,4]],[5,5]]") @?= 791

unit_2021_18_magnitude8 :: IO ()
unit_2021_18_magnitude8 = magnitude (readSnail "[[[[5,0],[7,4]],[5,5]],[6,6]]") @?= 1137

unit_2021_18_magnitude9 :: IO ()
unit_2021_18_magnitude9 = magnitude (readSnail "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") @?= 3488
