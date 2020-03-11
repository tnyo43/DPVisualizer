module ExRecursionFormula exposing (..)

import Array exposing (Array)
import Expect exposing (Expectation)
import Expr exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import RecursionFormula exposing (..)
import Test exposing (..)


testFixExpr : String -> Formula -> Formula -> Test
testFixExpr str f expected =
    test str <|
        \_ -> Expect.equal expected ( fix f )


testFixRecursionFormula : String -> Bool -> Int -> RecursionFormulas -> Test
testFixRecursionFormula str isInit row expected =
    test str <|
        \_ ->
            let
                fix = 
                    if isInit then fixInit else fixRecursion
            in
            Expect.equal expected ( fix row testRF )

testRF : RecursionFormulas
testRF =
    let
        init =
            [ FEditting "0" "0" "1" Array.empty |> Editting -- fixできる
            , FEditting "0" "0" "dp[0][0]" Array.empty |> Editting -- fixできない
            ]
            |> Array.fromList
        recursion =
            [ FEditting "0" "0" "1" (Array.fromList [("h", "1", "H")]) |> Editting -- fixできない
            , FEditting "0" "0" "dp[0][0]" Array.empty |> Editting -- fixできない
            , FEditting "h" "0" "dp[h-1][0] * 2" (Array.fromList [("h", "1", "H")]) |> Editting -- fixできる
            ]
            |> Array.fromList
    in
    RecursionFormulas init recursion

resultRF_init_0 : RecursionFormulas
resultRF_init_0 =
    let
        init =
            [ FFixed (Con 0) (Con 0) (Con 1) Array.empty |> Fixed
            , FEditting "0" "0" "dp[0][0]" Array.empty |> Editting
            ]
            |> Array.fromList
        recursion =
            [ FEditting "0" "0" "1" (Array.fromList [("h", "1", "H")]) |> Editting
            , FEditting "0" "0" "dp[0][0]" Array.empty |> Editting
            , FEditting "h" "0" "dp[h-1][0] * 2" (Array.fromList [("h", "1", "H")]) |> Editting
            ]
            |> Array.fromList
    in
    RecursionFormulas init recursion

resultRF_recursion_2 : RecursionFormulas
resultRF_recursion_2 =
    let
        init =
            [ FEditting "0" "0" "1" Array.empty |> Editting
            , FEditting "0" "0" "dp[0][0]" Array.empty |> Editting
            ]
            |> Array.fromList
        recursion =
            [ FEditting "0" "0" "1" (Array.fromList [("h", "1", "H")]) |> Editting
            , FEditting "0" "0" "dp[0][0]" Array.empty |> Editting
            , FFixed
                (Var "h") (Con 0)
                (App Mul (Dp (App Sub (Var "h") (Con 1)) (Con 0)) (Con 2))
                (Array.fromList [ For "h" (Con 1) (Var "H")]) |> Fixed
            ]
            |> Array.fromList
    in
    RecursionFormulas init recursion


suite : Test
suite =
    describe "The Recursion Formula"
        [ describe "try fix"
            [ testFixExpr
                "Fixedなら変更しない"
                ( FFixed (Con 0) (Con 0) (Con 1) Array.empty |> Fixed )
                ( FFixed (Con 0) (Con 0) (Con 1) Array.empty |> Fixed )
            , testFixExpr
                "dp[0][0] = 1 : success!"
                ( FEditting "0" "0" "1" Array.empty |> Editting )
                ( FFixed (Con 0) (Con 0) (Con 1) Array.empty |> Fixed )
            , testFixExpr
                "dp[i][j] = i + j : success!"
                ( FEditting "i" "j" "i+j" Array.empty |> Editting )
                ( FFixed (Var "i") (Var "j") (App Add (Var "i") (Var "j")) Array.empty |> Fixed )
            , testFixExpr
                "argsかtermでparseに失敗すると（i+）そのまま"
                ( FEditting "0" "i+" "i+j" Array.empty |> Editting )
                ( FEditting "0" "i+" "i+j" Array.empty |> Editting )
            ]
        , describe "initとrecursionのfix"
            [ testFixRecursionFormula
                "dpを含まないinitはfixできる"
                True 0 resultRF_init_0
            , testFixRecursionFormula
                "dpを含むinitはfixできない"
                True 1 testRF
            , testFixRecursionFormula
                "dpを含まないrecursionはfixできない"
                False 0 testRF
            , testFixRecursionFormula
                "forを含まないrecursionはfixできない"
                False 1 testRF
            , testFixRecursionFormula
                "forとdp項を含むrecursionはfixできる"
                False 2 resultRF_recursion_2
            ]
        ]
