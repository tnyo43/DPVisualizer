module Expr exposing (Term(..), Op(..), parse, eval, stringOf)

import Dict exposing (Dict)
import Parser exposing (..)
import Set exposing (empty)


type Op = Add | Sub | Mul | Div | Mod

type Term
    = App Op Term Term
    | Con Int
    | Var String
    | Dp Term Term


parser : Parser Term
parser =
    oneOf
        [ succeed (\term op expr -> App op term expr)
            |. backtrackable spaces
            |= backtrackable termParser
            |. spaces
            |= oneOf
                [ map (\_ -> Add) (symbol "+")
                , map (\_ -> Sub) (symbol "-")
                ]
            |. spaces
            |= lazy (\_ -> parser)
            |. spaces
        , termParser
        ]

termParser : Parser Term
termParser =
    oneOf
        [ succeed (\factor op term -> App op factor term)
            |. backtrackable spaces
            |= backtrackable factorParser
            |. backtrackable spaces
            |= oneOf
                [ map (\_ -> Mul) (symbol "*")
                , map (\_ -> Div) (symbol "/")
                , map (\_ -> Mod) (symbol "%")
                ]
            |. backtrackable spaces
            |= (lazy (\_ -> termParser))
            |. spaces
        , factorParser
        ]

factorParser : Parser Term
factorParser =
    oneOf
        [ succeed (\x -> x)
            |. backtrackable spaces
            |. symbol "("
            |. spaces
            |= (lazy (\_ -> parser))
            |. spaces
            |. symbol ")"
            |. spaces
        , succeed (\e1 e2 -> Dp e1 e2)
            |. backtrackable spaces
            |. symbol "dp["
            |. spaces
            |= (lazy (\_ -> parser))
            |. spaces
            |. symbol "]["
            |. spaces
            |= (lazy (\_ -> parser))
            |. spaces
            |. symbol "]"
        , map Con int
        , map Var
            ( variable
                { start = Char.isAlpha
                , inner = Char.isAlpha
                , reserved = Set.empty
                }
            )
        ]


parse : String ->  Result (List DeadEnd) Term
parse str =
    run parser str


eval : Dict String Int -> Term -> Maybe Int
eval dict trm =
    case trm of
        App op t1 t2 ->
            eval dict t1 |> Maybe.andThen (\v1 ->
            eval dict t2 |> Maybe.andThen (\v2 ->
                ( case op of
                    Add -> v1 + v2
                    Sub -> v1 - v2
                    Mul -> v1 * v2
                    Div -> v1 // v2
                    Mod -> modBy v2 v1
                ) |> Just
            ))
        Con n ->
            Just n
        Var v ->
            Dict.get v dict
        _ -> Just 0 -- TODO : implement eval dp table


stringOf : Term -> String
stringOf expr =
    case expr of
        App op t e ->
            let
                s1 = stringOf t
                sop =
                    case op of
                        Add -> " + "
                        Sub -> " - "
                        Mul -> " * "
                        Div -> " / "
                        Mod -> " % "
                s2 = stringOf e
            in
            s1 ++ sop ++ s2
        Con n ->
            String.fromInt n
        Var v ->
            v
        Dp e1 e2 ->
            "dp[" ++ (stringOf e1) ++ "][" ++ (stringOf e2) ++ "]"
