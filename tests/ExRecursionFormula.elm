module ExRecursionFormula exposing (..)

import Expect exposing (Expectation)
import Expr exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import RecursionFormula exposing (..)
import Test exposing (..)


testParseTerm : String -> Expr -> Test
testParseTerm str term =
    test str <|
        \_ -> Expect.equal ( Ok term ) ( parseExpr str )


testParseFail : String -> Test
testParseFail str =
    test str <|
        \_ -> Expect.err ( parseExpr str )


testApplyExpr : String -> RecursionFormula -> RecursionFormula -> Test
testApplyExpr str rf expected =
    test str <|
        \_ -> Expect.equal expected ( apply rf )


exprOfCon : Int -> Expr
exprOfCon n = Con n |> TFactor |> ETerm


exprOfVar : String -> Expr
exprOfVar v = Var v |> TFactor |> ETerm


suite : Test
suite =
    describe "The Recursion Formula"
        [ describe "expr parser"
            [ describe "parseが成功"
                [ testParseTerm "1" ( exprOfCon 1 )
                , testParseTerm "1 + x" ( AppExpr Add (TFactor (Con 1)) (ETerm (TFactor (Var "x"))) )
                , testParseTerm "1 - x" ( AppExpr Sub (TFactor (Con 1)) (ETerm (TFactor (Var "x"))) )
                , testParseTerm
                    "y % 4 + 2 / x"
                    ( AppExpr Add (AppTerm Mod (Var "y") (TFactor (Con 4))) (ETerm (AppTerm Div (Con 2) (TFactor (Var "x")))) )
                , testParseTerm
                    "y % (4 + 2) / x"
                    ( ETerm (AppTerm Mod (Var "y") (AppTerm Div (FExpr (AppExpr Add (TFactor (Con 4)) (ETerm (TFactor (Con 2))))) (TFactor (Var "x")))) )
                ]
            , describe "parseが失敗"
                [ testParseFail "+1"
                , testParseFail "1+"
                , testParseFail "1%"
                ]
            ]
        , describe "try apply"
            [ testApplyExpr
                "Appliedなら変更しない"
                ( RFApplied (exprOfCon 0) (exprOfCon 0) (exprOfCon 1) |> Applied )
                ( RFApplied (exprOfCon 0) (exprOfCon 0) (exprOfCon 1) |> Applied )
            , testApplyExpr
                "dp[0][0] = 1 : success!"
                ( RFEditting "0" "0" "1" |> Editting )
                ( RFApplied (exprOfCon 0) (exprOfCon 0) (exprOfCon 1) |> Applied )
            , testApplyExpr
                "dp[i][j] = i + j : success!"
                ( RFEditting "i" "j" "i+j" |> Editting )
                ( RFApplied (exprOfVar "i") (exprOfVar "j") (AppExpr Add (TFactor (Var "i")) (ETerm (TFactor (Var "j")))) |> Applied )
            , testApplyExpr
                "argsかtermでparseに失敗すると（i+）そのまま"
                ( RFEditting "0" "i+" "i+j" |> Editting )
                ( RFEditting "0" "i+" "i+j" |> Editting )
            ]
        ]
