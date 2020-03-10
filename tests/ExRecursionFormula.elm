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


testFixRecursionFormula : String -> Bool -> Int -> RecursionForumulas -> Test
testFixRecursionFormula str isInit row expected =
    test str <|
        \_ ->
            let
                fix = 
                    if isInit then fixInit else fixRecursion
            in
            Expect.equal expected ( fix row testRF )

testRF : RecursionForumulas
testRF =
    let
        init =
            [ FEditting "0" "0" "1" Array.empty |> Editting -- fixできる
            , FEditting "0" "0" "dp[0][0]" Array.empty |> Editting -- fixできない
            ]
            |> Array.fromList
        recursion =
            [ FEditting "0" "0" "1" Array.empty |> Editting -- fixできない
            , FEditting "0" "0" "dp[0][0]" Array.empty |> Editting -- fixできる
            ]
            |> Array.fromList
    in
    RecursionForumulas init recursion

resultRF_init_0 : RecursionForumulas
resultRF_init_0 =
    let
        init =
            [ FFixed (Con 0) (Con 0) (Con 1) Array.empty |> Fixed
            , FEditting "0" "0" "dp[0][0]" Array.empty |> Editting
            ]
            |> Array.fromList
        recursion =
            [ FEditting "0" "0" "1" Array.empty |> Editting
            , FEditting "0" "0" "dp[0][0]" Array.empty |> Editting
            ]
            |> Array.fromList
    in
    RecursionForumulas init recursion

resultRF_recursion_1 : RecursionForumulas
resultRF_recursion_1 =
    let
        init =
            [ FEditting "0" "0" "1" Array.empty |> Editting
            , FEditting "0" "0" "dp[0][0]" Array.empty |> Editting
            ]
            |> Array.fromList
        recursion =
            [ FEditting "0" "0" "1" Array.empty |> Editting
            , FFixed (Con 0) (Con 0) (Dp (Con 0) (Con 0)) Array.empty |> Fixed
            ]
            |> Array.fromList
    in
    RecursionForumulas init recursion


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
                "dpを含むrecursionはfixできる"
                False 1 resultRF_recursion_1
            ]
        ]
