module ExDPTable exposing (..)

import Array exposing (Array)
import DPTable exposing (..)
import Expect exposing (Expectation)
import Expr exposing (Term(..), Op(..))
import Fuzz exposing (Fuzzer, int, list, string)
import RecursionFormula exposing (..)
import Test exposing (..)


initTable_5_5 : () -> Table
initTable_5_5 _ =
    initTable 5 5


testApplyDPInit : String -> FFixed -> Table -> Test
testApplyDPInit str frm expected =
    test str <|
        \_ -> Expect.equal ( apply (initTable_5_5 ()) frm ) expected


editMultipleCels : Table -> List (Int, Int, Int) -> Table
editMultipleCels =
    List.foldl (\(i, j, n) t -> editTable i j n t)


suite : Test
suite =
    describe "DP Table"
        [ describe "DP初期条件で初期化"
            [ testApplyDPInit
                "dp[0][0] = 1"
                ( FFixed (Con 0) (Con 0) (Con 1) Array.empty )
                ( editMultipleCels (initTable_5_5 ()) [(0,0,1)] )
            , testApplyDPInit
                "dp[i][i] = 1は対角上が1"
                ( FFixed (Var "i") (Var "i") (Con 1) Array.empty )
                ( editMultipleCels (initTable_5_5 ()) [(0,0,1), (1,1,1), (2,2,1), (3,3,1), (4,4,1)] )
            , testApplyDPInit
                "dp[i][0] = 1はすべてのiに対して適用される"
                ( FFixed (Var "i") (Con 0) (Con 1) Array.empty )
                ( editMultipleCels (initTable_5_5 ()) [(0,0,1), (1,0,1), (2,0,1), (3,0,1), (4,0,1)] )
            , testApplyDPInit
                "dp[0][w] = 1はすべてのwに対して適用される"
                ( FFixed (Con 2) (Var "w") (Con 10) Array.empty )
                ( editMultipleCels (initTable_5_5 ()) [(2,0,10), (2,1,10), (2,2,10), (2,3,10), (2,4,10)] )
            , testApplyDPInit
                "dp[i][j] = 1はすべて1"
                ( FFixed (Var "i") (Var "j") (Con 1) Array.empty )
                ( editMultipleCels (initTable_5_5 ()) (List.range 0 25 |> List.map (\x -> (x // 5, modBy 5 x, 1))) )
            , testApplyDPInit
                "dp[i][0] = i, 引数にに使う変数はtermに使用できる"
                ( FFixed (Var "i") (Con 0) (Var "i") Array.empty )
                ( editMultipleCels (initTable_5_5 ()) [(0,0,0), (1,0,1), (2,0,2), (3,0,3), (4,0,4)] )
            , testApplyDPInit
                "dp[i][j] = i+j"
                ( FFixed (Var "i") (Var "j") ( App Add (Var "i") (Var "j") ) Array.empty )
                ( editMultipleCels (initTable_5_5 ()) (List.range 0 25 |> List.map (\x -> (x // 5, modBy 5 x, x // 5 + modBy 5 x))) )
            ]
        ]
