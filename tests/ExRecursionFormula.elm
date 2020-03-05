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
        [ testParseTerm "1" ( Con 1 )
        , testParseTerm "x" ( Var "x" )
        ]