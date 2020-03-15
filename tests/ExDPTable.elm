module ExDPTable exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import DPTable exposing (..)
import Expect exposing (Expectation)
import Expr exposing (Term(..), Op(..), For)
import Fuzz exposing (Fuzzer, int, list, string)
import RecursionFormula exposing (..)
import Test exposing (..)


initTable_5_5 : () -> Table
initTable_5_5 _ =
    initTable 5 5


initTableFromList : List (List Int) -> Table
initTableFromList lst =
    let
        h = List.length lst
        w = List.head lst |> Maybe.andThen (List.length >> Just) |> Maybe.withDefault 0
    in
    Table h w
        ( List.map (Array.fromList) lst
          |> Array.fromList
        )


editMultipleCels : Table -> List (Int, Int, Int) -> Table
editMultipleCels =
    List.foldl (\(i, j, n) t -> editTable i j n t)


testMakeForCombinations : String -> List For -> List ( List (String, Int) ) -> Test
testMakeForCombinations str fors expected =
    test str <|
        \_ -> Expect.equal ( Just expected ) ( makeForCombinations Array.empty Dict.empty fors )


testApplyDPInit : String -> FFixed -> Table -> Test
testApplyDPInit str frm expected =
    test str <|
        \_ -> Expect.equal expected ( applyFF frm (initTable_5_5 ()) )


testApplyRF : String -> RecursionFormulas -> Table -> Test
testApplyRF str rf expected =
    test str <|
        \_ -> Expect.equal expected ( apply rf (initTable_5_5 ()) )

makeRF : List FFixed -> List FFixed -> RecursionFormulas
makeRF init recursion =
    RecursionFormulas
        (List.map (\ff -> Fixed ff) init |> Array.fromList)
        (List.map (\ff -> Fixed ff) recursion |> Array.fromList)


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
                "for i = [0, 3), for j = [i, 2*i)" [ For "i" (Con 0) (Con 3),  For "j" (Var "i") (App Mul (Var "i") (Con 2)) ]
                [[("i",1),("j",1)], [("i",2),("j",2)], [("i",2),("j",3)]]
            , testMakeForCombinations
                "for n = [1, 5), for j = [1, n)" [ For "n" (Con 1) (Con 5), For "k" (Con 1) (Var "n") ]
                [[("n",2),("k",1)],[("n",3),("k",1)],[("n",3),("k",2)],[("n",4),("k",1)],[("n",4),("k",2)],[("n",4),("k",3)]]
            ]
        , describe "DP初期条件で初期化"
            [ testApplyDPInit
                "dp[0][0] = 1"
                ( FFixed (Con 0) (Con 0) (Con 1) Array.empty )
                ( editMultipleCels (initTable_5_5 ()) [(0,0,1)] )
            , testApplyDPInit
                "dp[i][i] = 1 (for i = [0, 5))は対角上が1"
                ( FFixed (Var "i") (Var "i") (Con 1) (Array.fromList [For "i" (Con 0) (Con 5)]) )
                ( editMultipleCels (initTable_5_5 ()) [(0,0,1), (1,1,1), (2,2,1), (3,3,1), (4,4,1)] )
            , testApplyDPInit
                "dp[i][0] = 1 (for i = [0, 3))はi=0,1,2で適用される"
                ( FFixed (Var "i") (Con 0) (Con 1) (Array.fromList [For "i" (Con 0) (Con 3)]) )
                ( editMultipleCels (initTable_5_5 ()) [(0,0,1), (1,0,1), (2,0,1), (0,0,1), (0,0,1)] )
            , testApplyDPInit
                "dp[i][0] = 1はすべてのiに対して適用される"
                ( FFixed (Var "i") (Con 0) (Con 1) (Array.fromList [For "i" (Con 0) (Con 5)]) )
                ( editMultipleCels (initTable_5_5 ()) [(0,0,1), (1,0,1), (2,0,1), (3,0,1), (4,0,1)] )
            , testApplyDPInit
                "dp[0][w] = 1はすべてのwに対して適用される"
                ( FFixed (Con 2) (Var "w") (Con 10) (Array.fromList [For "w" (Con 0) (Con 5)]) )
                ( editMultipleCels (initTable_5_5 ()) [(2,0,10), (2,1,10), (2,2,10), (2,3,10), (2,4,10)] )
            , testApplyDPInit
                "dp[i][j] = 1はすべて1"
                ( FFixed (Var "i") (Var "j") (Con 1) (Array.fromList [For "j" (Con 0) (Con 5), For "i" (Con 0) (Con 5)]) )
                ( editMultipleCels (initTable_5_5 ()) (List.range 0 25 |> List.map (\x -> (x // 5, modBy 5 x, 1))) )
            , testApplyDPInit
                "dp[i][0] = i, 引数にに使う変数はtermに使用できる"
                ( FFixed (Var "i") (Con 0) (Var "i") (Array.fromList [For "i" (Con 0) (Con 5)]) )
                ( editMultipleCels (initTable_5_5 ()) [(0,0,0), (1,0,1), (2,0,2), (3,0,3), (4,0,4)] )
            , testApplyDPInit
                "dp[i][j] = i+j"
                ( FFixed (Var "i") (Var "j") ( App Add (Var "i") (Var "j") ) (Array.fromList [For "j" (Con 0) (Con 5), For "i" (Con 0) (Con 5)]) )
                ( editMultipleCels (initTable_5_5 ()) (List.range 0 25 |> List.map (\x -> (x // 5, modBy 5 x, x // 5 + modBy 5 x))) )
            ]
        , describe "apply tests"
            [ testApplyRF
                "init: {dp[h][0] = 1}, recursion : {}"
                ( makeRF [ FFixed (Var "h") (Con 0) (Con 1) (Array.fromList [For "h" (Con 0) (Con 5)]) ] [] )
                ( editMultipleCels (initTable_5_5 ()) [(0,0,1), (1,0,1), (2,0,1), (3,0,1), (4,0,1)] )
            , testApplyRF
                "initは先頭から優先して適用 init: {dp[h][0] = 1, dp[0][w] = 2}, recursion : {}"
                ( makeRF
                    [ FFixed (Var "h") (Con 0) (Con 1) (Array.fromList [For "h" (Con 0) (Con 5)]), FFixed (Con 0) (Var "w") (Con 2) (Array.fromList [For "w" (Con 0) (Con 5)]) ]
                    []
                )
                ( editMultipleCels (initTable_5_5 ()) [(0,0,1), (1,0,1), (2,0,1), (3,0,1), (4,0,1), (0,1,2), (0,2,2), (0,3,2), (0,4,2)] )
            , testApplyRF
                "パスカルの三角形初期化"
                ( makeRF
                    [ FFixed (Var "n") (Con 0) (Con 1) (Array.fromList [For "n" (Con 0) (Con 5)])
                    , FFixed (Var "n") (Var "n") (Con 1) (Array.fromList [For "n" (Con 0) (Con 5)])
                    ]
                    []
                )
                ( initTableFromList [[1,0,0,0,0],[1,1,0,0,0],[1,0,1,0,0],[1,0,0,1,0],[1,0,0,0,1]] )
            , testApplyRF
                "パスカルの三角形"
                ( makeRF
                    [ FFixed (Var "n") (Con 0) (Con 1) (Array.fromList [For "n" (Con 0) (Con 5)])
                    , FFixed (Var "n") (Var "n") (Con 1) (Array.fromList [For "n" (Con 0) (Con 5)])
                    ]
                    [ FFixed (Var "n") (Var "k") (App Add (Dp (App Sub (Var "n") (Con 1)) (App Sub (Var "k") (Con 1))) (Dp (App Sub (Var "n") (Con 1)) (Var "k")))
                        (Array.fromList [For "n" (Con 1) (Var "H"), For "k" (Con 1) (Var "n")])
                    ]
                )
                ( initTableFromList [[1,0,0,0,0], [1,1,0,0,0], [1,2,1,0,0], [1,3,3,1,0], [1,4,6,4,1]] )
            ]
        ]
