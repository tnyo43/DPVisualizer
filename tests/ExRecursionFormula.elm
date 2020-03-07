module ExRecursionFormula exposing (..)

import Expect exposing (Expectation)
import Expr exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import RecursionFormula exposing (..)
import Test exposing (..)


testFixExpr : String -> Formula -> Formula -> Test
testFixExpr str f expected =
    test str <|
        \_ -> Expect.equal expected ( fix f )


suite : Test
suite =
    describe "The Recursion Formula"
        [ describe "try fix"
            [ testFixExpr
                "Fixedなら変更しない"
                ( FFixed (Con 0) (Con 0) (Con 1) |> Fixed )
                ( FFixed (Con 0) (Con 0) (Con 1) |> Fixed )
            , testFixExpr
                "dp[0][0] = 1 : success!"
                ( FEditting "0" "0" "1" |> Editting )
                ( FFixed (Con 0) (Con 0) (Con 1) |> Fixed )
            , testFixExpr
                "dp[i][j] = i + j : success!"
                ( FEditting "i" "j" "i+j" |> Editting )
                ( FFixed (Var "i") (Var "j") (App Add (Var "i") (Var "j")) |> Fixed )
            , testFixExpr
                "argsかtermでparseに失敗すると（i+）そのまま"
                ( FEditting "0" "i+" "i+j" |> Editting )
                ( FEditting "0" "i+" "i+j" |> Editting )
            ]
        ]
