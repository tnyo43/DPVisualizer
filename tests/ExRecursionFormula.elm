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
            [ makeEditting "0" "0" "1" Array.empty -- fixできる
            , makeEditting "0" "0" "dp[0][0]" Array.empty -- fixできない
            ]
            |> Array.fromList
        recursion =
            [ makeEditting "0" "0" "1" (Array.fromList [("h", "1", "H")]) -- fixできない
            , makeEditting "0" "0" "dp[0][0]" Array.empty -- fixできない
            , makeEditting "h" "0" "dp[h-1][0] * 2" (Array.fromList [("h", "1", "H")]) -- fixできる
            ]
            |> Array.fromList
    in
    RecursionFormulas init recursion

resultRF_init_0 : RecursionFormulas
resultRF_init_0 =
    let
        init =
            [ makeFixed (Con 0) (Con 0) (Con 1) Array.empty
            , makeEditting "0" "0" "dp[0][0]" Array.empty
            ]
            |> Array.fromList
        recursion =
            [ makeEditting "0" "0" "1" (Array.fromList [("h", "1", "H")])
            , makeEditting "0" "0" "dp[0][0]" Array.empty
            , makeEditting "h" "0" "dp[h-1][0] * 2" (Array.fromList [("h", "1", "H")])
            ]
            |> Array.fromList
    in
    RecursionFormulas init recursion

resultRF_recursion_2 : RecursionFormulas
resultRF_recursion_2 =
    let
        init =
            [ makeEditting "0" "0" "1" Array.empty
            , makeEditting "0" "0" "dp[0][0]" Array.empty
            ]
            |> Array.fromList
        recursion =
            [ makeEditting "0" "0" "1" (Array.fromList [("h", "1", "H")])
            , makeEditting "0" "0" "dp[0][0]" Array.empty
            , makeFixed
                (Var "h" []) (Con 0)
                (App Mul (Var "dp" [(App Sub (Var "h" []) (Con 1)), (Con 0)]) (Con 2))
                (Array.fromList [ For "h" (Con 1) (Var "H" [])])
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
                ( makeFixed (Con 0) (Con 0) (Con 1) Array.empty )
                ( makeFixed (Con 0) (Con 0) (Con 1) Array.empty )
            , testFixExpr
                "dp[0][0] = 1 : success!"
                ( makeEditting "0" "0" "1" Array.empty )
                ( makeFixed (Con 0) (Con 0) (Con 1) Array.empty )
            , testFixExpr
                "dp[i][j] = i + j : success!"
                ( makeEditting "i" "j" "i+j" Array.empty )
                ( makeFixed (Var "i" []) (Var "j" []) (App Add (Var "i" []) (Var "j" [])) Array.empty )
            , testFixExpr
                "argsかtermでparseに失敗すると（i+）そのまま"
                ( makeEditting "0" "i+" "i+j" Array.empty )
                ( makeEditting "0" "i+" "i+j" Array.empty )
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
