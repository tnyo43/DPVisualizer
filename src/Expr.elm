module Expr exposing (
            Variable, Value(..),
            Term(..), Op(..), UniOp(..), For,
            parse, parseFor, parseForArray, isIncludingDP, eval, stringOf
        )

import Array exposing (Array)
import Basics exposing (identity)
import Dict exposing (Dict)
import Parser exposing (..)
import Pratt exposing (..)
import Set exposing (Set)

type alias Variable = String

type Value
    = Num Int
    | Arr1 (Array Int)
    | Arr2 (Array (Array Int))

type Op = Add | Sub | Mul | Div | Mod

type UniOp = Neg

type Term
    = App Op Term Term
    | Uap UniOp Term
    | Con Int
    | Var Variable (List Term)

type alias For =
    { var : Variable
    , begin : Term
    , end : Term
    }


parenthesizedExpression : Config Term -> Parser Term
parenthesizedExpression config =
    succeed identity
        |. symbol "("
        |= subExpression 0 config
        |. symbol ")"


varExpression : Int -> Config Term -> Parser Term
varExpression arity config =
    let
        parseVar =
            map identity
                ( variable
                    { start = Char.isAlpha
                    , inner = Char.isAlpha
                    , reserved = Set.empty
                    }
                )
    in
    case arity of
        0 ->
            succeed (\v -> Var v [])
                |= parseVar
        1 ->
            succeed (\v t -> Var v [t])
                |= backtrackable parseVar
                |. backtrackable (symbol "[")
                |= backtrackable (subExpression 0 config)
                |. symbol "]"
        2 ->
            succeed (\v t1 t2 -> Var v [t1, t2])
                |= backtrackable parseVar
                |. backtrackable (symbol "[")
                |= backtrackable (subExpression 0 config)
                |. backtrackable (symbol "][")
                |= backtrackable (subExpression 0 config)
                |. symbol "]"
        _ ->
            problem "too many arguments"


parser : Parser Term
parser =
    succeed identity
        |= ( Pratt.expression
                { oneOf =
                    [ literal (map Con int)
                    , prefix 3 (symbol "-") (Uap Neg)
                    , varExpression 2
                    , varExpression 1
                    , varExpression 0
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
            )
        |. end


parse : Int -> String ->  Result (List DeadEnd) Term
parse dpDim str =
    run parser str
    |> Result.andThen (\trm ->
        case getVariables trm |> Set.member ("dp", 3 - dpDim) of
            True ->
                Err []
            False ->
                Ok trm
    )


parseFor : Int -> (String, String, String) -> Result (List DeadEnd) For
parseFor dpDim (var, begin, end) =
    if String.all Char.isAlpha var
    then
        parse dpDim begin |> Result.andThen (\b ->
        parse dpDim end |> Result.andThen (\e ->
            For var b e |> Ok
        ))
    else
        Err []


parseForArray : Int -> Array (String, String, String) -> Result (List DeadEnd) (Array For)
parseForArray dpDim fors =
    Array.foldl
        (\for ->
            Result.andThen (\array ->
                parseFor dpDim for
                |> Result.andThen (\f -> Array.push f array |> Ok)
            )
        )
        (Ok Array.empty)
        fors


getVariables : Term -> Set (Variable, Int)
getVariables trm =
    case trm of
        App _ t1 t2 ->
            Set.union (getVariables t1) (getVariables t2)
        Uap _ t ->
            getVariables t
        Con _ ->
            Set.empty
        Var v args ->
            List.map getVariables args
            |> List.foldl Set.union (Set.singleton (v, List.length args))


isIncludingDP : Term -> Bool
isIncludingDP trm =
    let
        set = getVariables trm
    in
    Set.member ("dp", 1) set || Set.member ("dp", 2) set


getFromTable : Array (Array Int) -> Int -> Int -> Maybe Int
getFromTable tbl h w =
    Array.get h tbl |> Maybe.andThen (\row ->
    Array.get w row |> Maybe.andThen (\val ->
        Just val
    ))


eval : Dict Variable Value -> Term -> Maybe Int
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
        Uap _ t ->
            eval dict t
            |> Maybe.andThen (\v -> Just (-v))
        Con n ->
            Just n
        Var v args_ ->
            let
                maybeList lst =
                    if List.member Nothing lst then Nothing
                    else List.filterMap identity lst |> Just
            in
            List.map (eval dict) args_ |> maybeList |> Maybe.andThen (\args ->
            Dict.get v dict |> Maybe.andThen (\value ->
                case ( value, args ) of
                    ( Num n, [] ) ->
                        Just n
                    ( Arr1 ns, [a1] ) ->
                        Array.get a1 ns
                    ( Arr2 nss, [a1, a2] ) ->
                        Array.get a1 nss |> Maybe.andThen (\ns -> Array.get a2 ns)
                    _ -> Nothing
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
        Var v args ->
            v ++ (List.map (\a -> "[" ++ stringOf a ++ "]") args |> String.concat)
