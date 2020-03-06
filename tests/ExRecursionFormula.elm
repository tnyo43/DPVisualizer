module ExRecursionFormula exposing (..)

import Expect exposing (Expectation)
import Expr exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import RecursionFormula exposing (..)
import Test exposing (..)


testParseTerm : String -> Term -> Test
testParseTerm str term =
    test str <|
        \_ -> Expect.equal ( Ok term ) ( parse str )


testParseFail : String -> Test
testParseFail str =
    test str <|
        \_ -> Expect.err ( parse str )


testFixExpr : String -> RecursionFormula -> RecursionFormula -> Test
testFixExpr str rf expected =
    test str <|
        \_ -> Expect.equal expected ( fix rf )


suite : Test
suite =
    describe "The Recursion Formula"
        [ describe "expr parser"
            [ describe "parseが成功"
                [ testParseTerm "1" ( Con 1 )
                , testParseTerm "1 + x" ( App Add (Con 1) (Var "x") )
                , testParseTerm "1 - x" ( App Sub (Con 1) (Var "x") )
                , testParseTerm
                    "y % 4 + 2 / x"
                    ( App Add (App Mod (Var "y") (Con 4)) (App Div (Con 2) (Var "x")) )
                , testParseTerm
                    "dp[i][j]"
                    ( Dp (Var "i") (Var "j") )
                , testParseTerm
                    "dp[i*2][j+1] % i + 2 * dp[i][j-1]"
                    ( App Add
                        ( App Mod (Dp (App Mul (Var "i") (Con 2)) (App Add (Var "j") (Con 1))) (Var "i") )
                        ( App Mul (Con 2) (Dp (Var "i") (App Sub (Var "j") (Con 1))) )
                    )
                ]
            , describe "parseが失敗"
                [ testParseFail "+1"
                , testParseFail "1+"
                , testParseFail "1%"
                ]
            ]
        , describe "try fix"
            [ testFixExpr
                "Fixedなら変更しない"
                ( RFFixed (Con 0) (Con 0) (Con 1) |> Fixed )
                ( RFFixed (Con 0) (Con 0) (Con 1) |> Fixed )
            , testFixExpr
                "dp[0][0] = 1 : success!"
                ( RFEditting "0" "0" "1" |> Editting )
                ( RFFixed (Con 0) (Con 0) (Con 1) |> Fixed )
            , testFixExpr
                "dp[i][j] = i + j : success!"
                ( RFEditting "i" "j" "i+j" |> Editting )
                ( RFFixed (Var "i") (Var "j") (App Add (Var "i") (Var "j")) |> Fixed )
            , testFixExpr
                "argsかtermでparseに失敗すると（i+）そのまま"
                ( RFEditting "0" "i+" "i+j" |> Editting )
                ( RFEditting "0" "i+" "i+j" |> Editting )
            ]
        ]
