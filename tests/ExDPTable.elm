module ExDPTable exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import DPTable exposing (..)
import Expect exposing (Expectation)
import Expr exposing (Variable, Term(..), Op(..), For)
import Fuzz exposing (Fuzzer, int, list, string)
import RecursionFormula exposing (..)
import Test exposing (..)


testMakeForCombinations : String -> List For -> List ( List (Variable, Int) ) -> Test
testMakeForCombinations str fors expected =
    test str <|
        \_ -> Expect.equal ( Just expected ) ( makeForCombinations Dict.empty fors )


makeRF : List Formula -> List Formula -> RecursionFormulas
makeRF init recursion =
    RecursionFormulas
        (Array.fromList init)
        (Array.fromList recursion)


-- Test D1

initTable_10 : () -> Table
initTable_10 _ =
    initTable 10 Nothing


initTableD1FromList : List Int -> Table
initTableD1FromList lst =
    Array.fromList lst
    |> makeTableD1 (List.length lst) 


editMultipleCelsD1 : Table -> List (Int, Int) -> Table
editMultipleCelsD1 =
    List.foldl (\(i, n) t -> editTable i Nothing n t)


testApplyRFD1 : String -> RecursionFormulas -> Table -> Test
testApplyRFD1 str rf expected =
    test str <|
        \_ -> Expect.equal expected ( apply Dict.empty rf (initTable_10 ()) )


-- TestD2

initTable_5_5 : () -> Table
initTable_5_5 _ =
    initTable 5 (Just 5)


initTableD2FromList : List (List Int) -> Table
initTableD2FromList lst =
    let
        h = List.length lst
        w = List.head lst |> Maybe.andThen (List.length >> Just) |> Maybe.withDefault 0
    in
    List.map (Array.fromList) lst
    |> Array.fromList
    |> makeTableD2 h w 


editMultipleCelsD2 : Table -> List (Int, Int, Int) -> Table
editMultipleCelsD2 =
    List.foldl (\(i, j, n) t -> editTable i (Just j) n t)


testApplyDPInitD2 : String -> Formula -> Table -> Test
testApplyDPInitD2 str frm expected =
    test str <|
        \_ -> Expect.equal expected ( applyFF Dict.empty frm (initTable_5_5 ()) )


testApplyRFD2 : String -> RecursionFormulas -> Table -> Test
testApplyRFD2 str rf expected =
    test str <|
        \_ -> Expect.equal expected ( apply Dict.empty rf (initTable_5_5 ()) )


suite : Test
suite =
    describe "DP Table"
        [ describe "Forの変数の組み合わせ"
            [ testMakeForCombinations
                "for i = [0, 5)" [ For "i" (Con 0) (Con 5) ]
                [[("i", 0)], [("i", 1)], [("i", 2)], [("i", 3)], [("i", 4)]]
            , testMakeForCombinations
                "for i = [0, 2), for j = [2, 4)" [ For "i" (Con 0) (Con 2),  For "j" (Con 2) (Con 4) ]
                [[("i",0),("j",2)], [("i",0),("j",3)], [("i",1),("j",2)], [("i",1),("j",3)]]
            , testMakeForCombinations
                "for i = [0, 3), for j = [i, 2*i)" [ For "i" (Con 0) (Con 3),  For "j" (Var "i" []) (App Mul (Var "i" []) (Con 2)) ]
                [[("i",1),("j",1)], [("i",2),("j",2)], [("i",2),("j",3)]]
            , testMakeForCombinations
                "for n = [1, 5), for j = [1, n)" [ For "n" (Con 1) (Con 5), For "k" (Con 1) (Var "n" []) ]
                [[("n",2),("k",1)],[("n",3),("k",1)],[("n",3),("k",2)],[("n",4),("k",1)],[("n",4),("k",2)],[("n",4),("k",3)]]
            ]
        , describe "DP初期条件で初期化"
            [ testApplyDPInitD2
                "dp[0][0] = 1"
                ( makeFixed (Con 0) (Con 0 |> Just) (Con 1) Array.empty )
                ( editMultipleCelsD2 (initTable_5_5 ()) [(0,0,1)] )
            , testApplyDPInitD2
                "dp[i][i] = 1 (for i = [0, 5))は対角上が1"
                ( makeFixed (Var "i" []) (Var "i" [] |> Just) (Con 1) (Array.fromList [For "i" (Con 0) (Con 5)]) )
                ( editMultipleCelsD2 (initTable_5_5 ()) [(0,0,1), (1,1,1), (2,2,1), (3,3,1), (4,4,1)] )
            , testApplyDPInitD2
                "dp[i][0] = 1 (for i = [0, 3))はi=0,1,2で適用される"
                ( makeFixed (Var "i" []) (Con 0 |> Just) (Con 1) (Array.fromList [For "i" (Con 0) (Con 3)]) )
                ( editMultipleCelsD2 (initTable_5_5 ()) [(0,0,1), (1,0,1), (2,0,1), (0,0,1), (0,0,1)] )
            , testApplyDPInitD2
                "dp[i][0] = 1はすべてのiに対して適用される"
                ( makeFixed (Var "i" []) (Con 0 |> Just) (Con 1) (Array.fromList [For "i" (Con 0) (Con 5)]) )
                ( editMultipleCelsD2 (initTable_5_5 ()) [(0,0,1), (1,0,1), (2,0,1), (3,0,1), (4,0,1)] )
            , testApplyDPInitD2
                "dp[0][w] = 1はすべてのwに対して適用される"
                ( makeFixed (Con 2) (Var "w" [] |> Just) (Con 10) (Array.fromList [For "w" (Con 0) (Con 5)]) )
                ( editMultipleCelsD2 (initTable_5_5 ()) [(2,0,10), (2,1,10), (2,2,10), (2,3,10), (2,4,10)] )
            , testApplyDPInitD2
                "dp[i][j] = 1はすべて1"
                ( makeFixed (Var "i" []) (Var "j" [] |> Just) (Con 1) (Array.fromList [For "j" (Con 0) (Con 5), For "i" (Con 0) (Con 5)]) )
                ( editMultipleCelsD2 (initTable_5_5 ()) (List.range 0 25 |> List.map (\x -> (x // 5, modBy 5 x, 1))) )
            , testApplyDPInitD2
                "dp[i][0] = i, 引数にに使う変数はtermに使用できる"
                ( makeFixed (Var "i" []) (Con 0 |> Just) (Var "i" []) (Array.fromList [For "i" (Con 0) (Con 5)]) )
                ( editMultipleCelsD2 (initTable_5_5 ()) [(0,0,0), (1,0,1), (2,0,2), (3,0,3), (4,0,4)] )
            , testApplyDPInitD2
                "dp[i][j] = i+j"
                ( makeFixed (Var "i" []) (Var "j" [] |> Just) ( App Add (Var "i" []) (Var "j" []) ) (Array.fromList [For "j" (Con 0) (Con 5), For "i" (Con 0) (Con 5)]) )
                ( editMultipleCelsD2 (initTable_5_5 ()) (List.range 0 25 |> List.map (\x -> (x // 5, modBy 5 x, x // 5 + modBy 5 x))) )
            ]
        , describe "apply tests"
            [ testApplyRFD2
                "init: {dp[h][0] = 1}, recursion : {}"
                ( makeRF [ makeFixed (Var "h" []) (Con 0 |> Just) (Con 1) (Array.fromList [For "h" (Con 0) (Con 5)]) ] [] )
                ( editMultipleCelsD2 (initTable_5_5 ()) [(0,0,1), (1,0,1), (2,0,1), (3,0,1), (4,0,1)] )
            , testApplyRFD2
                "initは先頭から優先して適用 init: {dp[h][0] = 1, dp[0][w] = 2}, recursion : {}"
                ( makeRF
                    [ makeFixed (Var "h" []) (Con 0 |> Just) (Con 1) (Array.fromList [For "h" (Con 0) (Con 5)])
                    , makeFixed (Con 0) (Var "w" [] |> Just) (Con 2) (Array.fromList [For "w" (Con 0) (Con 5)]) ]
                    []
                )
                ( editMultipleCelsD2 (initTable_5_5 ()) [(0,0,1), (1,0,1), (2,0,1), (3,0,1), (4,0,1), (0,1,2), (0,2,2), (0,3,2), (0,4,2)] )
            , testApplyRFD2
                "パスカルの三角形初期化"
                ( makeRF
                    [ makeFixed (Var "n" []) (Con 0 |> Just) (Con 1) (Array.fromList [For "n" (Con 0) (Con 5)])
                    , makeFixed (Var "n" []) (Var "n" [] |> Just) (Con 1) (Array.fromList [For "n" (Con 0) (Con 5)])
                    ]
                    []
                )
                ( initTableD2FromList [[1,0,0,0,0],[1,1,0,0,0],[1,0,1,0,0],[1,0,0,1,0],[1,0,0,0,1]] )
            , testApplyRFD2
                "パスカルの三角形"
                ( makeRF
                    [ makeFixed (Var "n" []) (Con 0 |> Just) (Con 1) (Array.fromList [For "n" (Con 0) (Con 5)])
                    , makeFixed (Var "n" []) (Var "n" [] |> Just) (Con 1) (Array.fromList [For "n" (Con 0) (Con 5)])
                    ]
                    [ makeFixed
                        (Var "n" []) (Var "k" [] |> Just)
                        (App Add (Var "dp" [(App Sub (Var "n" []) (Con 1)), (App Sub (Var "k" []) (Con 1))]) (Var "dp" [(App Sub (Var "n" []) (Con 1)), (Var "k" [])]))
                        (Array.fromList [For "n" (Con 1) (Var "H" []), For "k" (Con 1) (Var "n" [])])
                    ]
                )
                ( initTableD2FromList [[1,0,0,0,0], [1,1,0,0,0], [1,2,1,0,0], [1,3,3,1,0], [1,4,6,4,1]] )
            ]
        , describe "apply tests 1D"
            [ testApplyRFD1
                "フィボナッチ数列"
                ( makeRF
                    [ makeFixed (Var "n" []) Nothing (Con 1) (Array.fromList [For "n" (Con 0) (Con 2)]) ]
                    [ makeFixed
                        (Var "n" []) Nothing
                        (App Add (Var "dp" [App Sub (Var "n" []) (Con 1)]) (Var "dp" [App Sub (Var "n" []) (Con 2)]))
                        (Array.fromList [For "n" (Con 2) (Var "N" [])])
                    ]
                )
                ( initTableD1FromList [1,1,2,3,5,8,13,21,34,55] )
            ]
        ]
