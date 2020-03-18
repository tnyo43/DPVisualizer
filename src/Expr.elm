module Expr exposing (Term(..), Op(..), UniOp(..), For, parse, parseFor, parseForArray, isIncludingDP, eval, stringOf)

import Array exposing (Array)
import Basics exposing (identity)
import Dict exposing (Dict)
import Parser exposing (..)
import Pratt exposing (..)
import Set exposing (empty)


type Op = Add | Sub | Mul | Div | Mod

type UniOp = Neg

type Term
    = App Op Term Term
    | Uap UniOp Term
    | Con Int
    | Var String
    | Dp Term Term

type alias For =
    { var : String
    , begin : Term
    , end : Term
    }


dpExpression : Config Term -> Parser Term
dpExpression config =
    succeed Dp
        |= prefix 0 (symbol "dp[") identity config
        |= prefix 0 (symbol "][") identity config
        |. symbol "]"


parenthesizedExpression : Config Term -> Parser Term
parenthesizedExpression config =
    succeed identity
        |. symbol "("
        |= subExpression 0 config
        |. symbol ")"


varExpression : Config Term -> Parser Term
varExpression _ =
    map Var
        ( variable
            { start = Char.isAlpha
            , inner = Char.isAlpha
            , reserved = Set.empty
            }
        )


parser : Parser Term
parser =
    Pratt.expression
        { oneOf =
            [ literal (map Con int)
            , prefix 3 (symbol "-") (Uap Neg)
            , dpExpression
            , varExpression
            , parenthesizedExpression
            ]
        , andThenOneOf =
            [ infixLeft 1 (symbol "+") (App Add)
            , infixLeft 1 (symbol "-") (App Sub)
            , infixLeft 2 (symbol "*") (App Mul)
            , infixLeft 2 (symbol "/") (App Div)
            , infixLeft 2 (symbol "%") (App Mod)
            ]
        , spaces = Parser.spaces
        }


parse : String ->  Result (List DeadEnd) Term
parse str =
    run parser str


parseFor : (String, String, String) -> Result (List DeadEnd) For
parseFor (var, begin, end) =
    if String.all Char.isAlpha var
    then
        run parser begin |> Result.andThen (\b ->
        run parser end |> Result.andThen (\e ->
            For var b e |> Ok
        ))
    else
        Err []


parseForArray : Array (String, String, String) -> Result (List DeadEnd) (Array For)
parseForArray fors =
    Array.foldl
        (\for ->
            Result.andThen (\array ->
                parseFor for
                |> Result.andThen (\f -> Array.push f array |> Ok)
            )
        )
        (Ok Array.empty)
        fors


isIncludingDP : Term -> Bool
isIncludingDP trm =
    case trm of
        Dp _ _ ->
            True
        App op t1 t2 ->
            isIncludingDP t1 || isIncludingDP t2
        _ -> False


getFromTable : Array (Array Int) -> Int -> Int -> Maybe Int
getFromTable tbl h w =
    Array.get h tbl |> Maybe.andThen (\row ->
    Array.get w row |> Maybe.andThen (\val ->
        Just val
    ))


eval : Array (Array Int) -> Dict String Int -> Term -> Maybe Int
eval dp dict trm =
    case trm of
        App op t1 t2 ->
            eval dp dict t1 |> Maybe.andThen (\v1 ->
            eval dp dict t2 |> Maybe.andThen (\v2 ->
                ( case op of
                    Add -> v1 + v2
                    Sub -> v1 - v2
                    Mul -> v1 * v2
                    Div -> v1 // v2
                    Mod -> modBy v2 v1
                ) |> Just
            ))
        Uap _ t ->
            eval dp dict t
            |> Maybe.andThen (\v -> Just (-v))
        Con n ->
            Just n
        Var v ->
            Dict.get v dict
        Dp th tw ->
            eval dp dict th |> Maybe.andThen (\h ->
            eval dp dict tw |> Maybe.andThen (\w ->
                getFromTable dp h w
            ))


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
        Uap _ t ->
            "-" ++ stringOf t
        Con n ->
            String.fromInt n
        Var v ->
            v
        Dp e1 e2 ->
            "dp[" ++ (stringOf e1) ++ "][" ++ (stringOf e2) ++ "]"
