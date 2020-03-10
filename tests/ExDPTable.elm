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


testMakeForCombinations : String -> List For -> List ( List (String, Int) ) -> Test
testMakeForCombinations str fors expected =
    test str <|
        \_ -> Expect.equal ( Just expected ) ( makeForCombinations Array.empty Dict.empty fors )


testApplyDPInit : String -> FFixed -> Table -> Test
testApplyDPInit str frm expected =
    test str <|
        \_ -> Expect.equal expected ( apply (initTable_5_5 ()) frm )


editMultipleCels : Table -> List (Int, Int, Int) -> Table
editMultipleCels =
    List.foldl (\(i, j, n) t -> editTable i j n t)


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
        ]
