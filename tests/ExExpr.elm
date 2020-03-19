module ExExpr exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Expr exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


testParseTerm : String -> Term -> Test
testParseTerm str term =
    test str <|
        \_ -> Expect.equal ( Ok term ) ( parse str )


testParseFail : String -> Test
testParseFail str =
    test str <|
        \_ -> Expect.err ( parse str )


testParseFor : ( String, String, String ) -> For -> Test
testParseFor (v, b, e) for =
    test (v ++ ", " ++ b ++ ", " ++ e) <|
        \_ -> Expect.equal ( Ok for ) ( parseFor (v, b, e) )


testParseForFail : ( String, String, String ) -> Test
testParseForFail (v, b, e) =
    test (v ++ ", " ++ b ++ ", " ++ e) <|
        \_ -> Expect.err ( parseFor (v, b, e) )


testParseForArray : Array ( String, String, String ) -> Array For -> Test
testParseForArray fors expected =
    test (Array.foldl (\(v,b,e) acc -> acc ++ " [" ++ (v ++ ", " ++ b ++ ", " ++ e ++ "]") ) "" fors) <|
        \_ -> Expect.equal ( Ok expected ) ( parseForArray fors )


testParseForArrayFail : Array ( String, String, String ) -> Test
testParseForArrayFail fors =
    test (Array.foldl (\(v,b,e) acc -> acc ++ " [" ++ (v ++ ", " ++ b ++ ", " ++ e ++ "]") ) "" fors) <|
        \_ -> Expect.err ( parseForArray fors )


testDict : Dict Variable Value
testDict =
    [ ( "a", Num 1 )
    , ( "b", Num 2 )
    , ( "c", Num 3 )
    , ( "d", Num 4 )
    , ( "e", Num 5 )
    , ( "f", Num 6 )
    , ( "dp", Arr2 testDPTable )
    ]
    |> Dict.fromList


testDPTable : Array (Array Int)
testDPTable =
    Array.fromList
        [ Array.fromList [1, 2, 3, 4, 5]
        , Array.fromList [2, 3, 4, 5, 6]
        , Array.fromList [3, 4, 5, 6, 7]
        , Array.fromList [4, 5, 6, 7, 8]
        , Array.fromList [5, 6, 7, 8, 9]
        ]


testEval : String -> Term -> Maybe Int -> Test
testEval str trm expected =
    test str <|
        \_ -> Expect.equal ( eval testDict trm ) expected


suite : Test
suite = describe "Test Expr"
    [ describe "expr parser"
        [ describe "parseが成功"
            [ testParseTerm "1" ( Con 1 )
            , testParseTerm "1 + x" ( App Add (Con 1) (Var "x" []) )
            , testParseTerm "1 - x" ( App Sub (Con 1) (Var "x" []) )
            , testParseTerm
                "y % 4 + 2 / x"
                ( App Add (App Mod (Var "y" []) (Con 4)) (App Div (Con 2) (Var "x" [])) )
            , testParseTerm
                "dp[i][j]"
                ( Var "dp" [Var "i" [], Var "j" []] )
            , testParseTerm
                "dp[i*2][j+1] % i + 2 * dp[i][j-1]"
                ( App Add
                    ( App Mod (Var "dp" [App Mul (Var "i" []) (Con 2), App Add (Var "j" []) (Con 1)]) (Var "i" []) )
                    ( App Mul (Con 2) (Var "dp" [Var "i" [], App Sub (Var "j" []) (Con 1)]) )
                )
            , testParseTerm
                "a - b - c"
                ( App Sub
                    ( App Sub (Var "a" []) (Var "b" []) )
                    ( Var "c" [] )
                )
            , testParseTerm
                "-1"
                ( Uap Neg (Con 1) )
            , testParseTerm
                "-a"
                ( Uap Neg (Var "a" []) )
            ]
        , describe "parseが失敗"
            [ testParseFail "+1"
            , testParseFail "1+"
            , testParseFail "1%"
            , testParseFail "dp[hoge][fuga"
            ]
        ]
    , describe "for parser"
        [ describe "parseが成功"
            [ testParseFor ("i", "0", "10") (For "i" (Con 0) (Con 10))
            , testParseFor ("w", "a", "b + 1") (For "w" (Var "a" []) (App Add (Var "b" []) (Con 1)))
            ]
        , describe "parseが失敗"
            [ testParseForFail ("0", "0", "0")
            , testParseForFail ("i+1", "0", "0")
            , testParseForFail ("dp[0][0]", "0", "0")
            ]
        , describe "for arrayのparse"
            [ describe "全て成功すると成功"
                [ testParseForArray
                    ( Array.fromList [("i", "0", "10"), ("w", "a", "b + 1")] )
                    ( Array.fromList [(For "i" (Con 0) (Con 10)), (For "w" (Var "a" []) (App Add (Var "b" []) (Con 1)))] )
                ]
            , describe "一つでも失敗すると失敗"
                [ testParseForArrayFail ( Array.fromList [("i+1", "0", "10"), ("w", "a", "b + 1")] )
                ]
            ]
        ]
    , describe "eval"
        [ describe "evalが成功する"
            [ testEval "10" ( Con 10 ) ( Just 10 )
            , testEval "a => 1" ( Var "a" [] ) ( Just 1 )
            , testEval "b + 1 => 2 + 1 => 3" ( App Add (Var "b" []) (Con 1) ) ( Just 3 )
            , testEval "c + d * 2 - e => 3 + 4 * 2 - 5 => " ( App Sub ( App Add ( Var "c" [] ) ( App Mul ( Var "d" [] ) ( Con 2 ) )) ( Var "e" [] ) ) ( Just 6 )
            , testEval "f % 4 => 2" ( App Mod ( Var "f" [] ) ( Con 4 ) ) ( Just 2 )
            , testEval "dp[0][0] = 1" ( Var "dp" [Con 0, Con 0] ) ( Just 1 )
            , testEval "dp[5][0]は配列外なのでNothing" ( Var "dp" [Con 5, Con 0] ) Nothing
            , testEval "dp[2][3] + dp[4][4] = " ( App Sub (Var "dp" [Con 2, Con 3]) (Var "dp" [Con 4, Con 4]) ) ( Just (-3) )
            ]
        , describe "evalが失敗する"
            [ testEval "xは存在しない" ( Var "x" [] ) Nothing
            , testEval "a + y + b, yは存在しない" ( App Add (Var "a" []) ( App Add (Var "y" []) (Var "b" [])) ) Nothing
            ]
        ]
    ]