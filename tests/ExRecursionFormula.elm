module ExRecursionFormula exposing (..)

import Array exposing (Array)
import Expect exposing (Expectation)
import Expr exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import RecursionFormula exposing (..)
import Test exposing (..)


testFixExprD2 : String -> Formula -> Formula -> Test
testFixExprD2 str f expected =
    test str <|
        \_ -> Expect.equal expected ( fix 2 f )


testFixRecursionFormulaD2 : String -> Bool -> Int -> RecursionFormulas -> Test
testFixRecursionFormulaD2 str isInit row expected =
    test str <|
        \_ ->
            let
                fix = 
                    if isInit then fixInit else fixRecursion
            in
            Expect.equal expected ( fix 2 row testRF )


testRF : RecursionFormulas
testRF =
    let
        init =
            [ makeEditting "0" (Just "0") "1" Array.empty -- fixできる
            , makeEditting "0" (Just "0") "dp[0][0]" Array.empty -- fixできない
            ]
            |> Array.fromList
        recursion =
            [ makeEditting "0" (Just "0") "1" (Array.fromList [("h", "1", "H")]) -- fixできない
            , makeEditting "0" (Just "0") "dp[0][0]" Array.empty -- fixできない
            , makeEditting "h" (Just "0") "dp[h-1][0] * 2" (Array.fromList [("h", "1", "H")]) -- fixできる
            ]
            |> Array.fromList
    in
    RecursionFormulas init recursion

resultRF_init_0 : RecursionFormulas
resultRF_init_0 =
    let
        init =
            [ makeFixed (Con 0) (Con 0 |> Just) (Con 1) Array.empty
            , makeEditting "0" (Just "0") "dp[0][0]" Array.empty
            ]
            |> Array.fromList
        recursion =
            [ makeEditting "0" (Just "0") "1" (Array.fromList [("h", "1", "H")])
            , makeEditting "0" (Just "0") "dp[0][0]" Array.empty
            , makeEditting "h" (Just "0") "dp[h-1][0] * 2" (Array.fromList [("h", "1", "H")])
            ]
            |> Array.fromList
    in
    RecursionFormulas init recursion

resultRF_recursion_2 : RecursionFormulas
resultRF_recursion_2 =
    let
        init =
            [ makeEditting "0" (Just "0") "1" Array.empty
            , makeEditting "0" (Just "0") "dp[0][0]" Array.empty
            ]
            |> Array.fromList
        recursion =
            [ makeEditting "0" (Just "0") "1" (Array.fromList [("h", "1", "H")])
            , makeEditting "0" (Just "0") "dp[0][0]" Array.empty
            , makeFixed
                (Var "h" []) (Con 0 |> Just)
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
            [ testFixExprD2
                "Fixedなら変更しない"
                ( makeFixed (Con 0) (Con 0 |> Just) (Con 1) Array.empty )
                ( makeFixed (Con 0) (Con 0 |> Just) (Con 1) Array.empty )
            , testFixExprD2
                "dp[0][0] = 1 : success!"
                ( makeEditting "0" (Just "0") "1" Array.empty )
                ( makeFixed (Con 0) (Con 0 |> Just) (Con 1) Array.empty )
            , testFixExprD2
                "dp[i][j] = i + j : success!"
                ( makeEditting "i" (Just "j") "i+j" Array.empty )
                ( makeFixed (Var "i" []) (Var "j" [] |> Just) (App Add (Var "i" []) (Var "j" [])) Array.empty )
            , testFixExprD2
                "argsかtermでparseに失敗すると（i+）そのまま"
                ( makeEditting "0" (Just "i+") "i+j" Array.empty )
                ( makeEditting "0" (Just "i+") "i+j" Array.empty )
            ]
        , describe "initとrecursionのfix"
            [ testFixRecursionFormulaD2
                "dpを含まないinitはfixできる"
                True 0 resultRF_init_0
            , testFixRecursionFormulaD2
                "dpを含むinitはfixできない"
                True 1 testRF
            , testFixRecursionFormulaD2
                "dpを含まないrecursionはfixできない"
                False 0 testRF
            , testFixRecursionFormulaD2
                "forを含まないrecursionはfixできない"
                False 1 testRF
            , testFixRecursionFormulaD2
                "forとdp項を含むrecursionはfixできる"
                False 2 resultRF_recursion_2
            ]
        ]
