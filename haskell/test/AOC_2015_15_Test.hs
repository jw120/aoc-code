module AOC_2015_15_Test where

import Test.Tasty.HUnit ((@?=))

import AOC_2015_15 (Ingredient, bestCookie, cookieScore, pIngredient)
import Utilities (parseOrStop)

testIngredients :: [Ingredient]
testIngredients =
    map
        (parseOrStop pIngredient)
        [ "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
        , "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
        ]

testCookie :: [(Ingredient, Int)]
testCookie = zip testIngredients [44, 56]

unit_2015_15_cookie_score :: IO ()
unit_2015_15_cookie_score = cookieScore testCookie @?= 62842880

unit_2015_15_cookie_score1 :: IO ()
unit_2015_15_cookie_score1 = cookieScore (zip testIngredients [1, 0]) @?= 0

unit_2015_15_cookie_score2 :: IO ()
unit_2015_15_cookie_score2 = cookieScore (zip testIngredients [0, 1]) @?= 0

unit_2015_15_cookie_ingredients :: IO ()
unit_2015_15_cookie_ingredients = bestCookie testIngredients @?= 62842880
