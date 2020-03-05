module ExRecursionFormula exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import RecursionFormula exposing (..)
import Test exposing (..)


testParseTerm : String -> Expr -> Test
testParseTerm str term =
    test str <|
        \_ -> Expect.equal (Ok term) ( parseExpr str )


suite : Test
suite =
    describe "The Recursion Formula"
        [ testParseTerm "1" ( Con 1 |> TFactor |> ETerm )
        , testParseTerm "1 + x" ( AppExpr Add (TFactor (Con 1)) (ETerm (TFactor (Var "x"))) )
        , testParseTerm "1 - x" ( AppExpr Sub (TFactor (Con 1)) (ETerm (TFactor (Var "x"))) )
        , testParseTerm
            "y % 4 + 2 / x"
            ( AppExpr Add (AppTerm Mod (Var "y") (TFactor (Con 4))) (ETerm (AppTerm Div (Con 2) (TFactor (Var "x")))) )
        , testParseTerm
            "y % (4 + 2) / x"
            ( ETerm (AppTerm Mod (Var "y") (AppTerm Div (FExpr (AppExpr Add (TFactor (Con 4)) (ETerm (TFactor (Con 2))))) (TFactor (Var "x")))) )
        ]
