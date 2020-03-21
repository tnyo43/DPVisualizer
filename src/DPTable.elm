module DPTable exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Expr exposing (Variable, Value(..), Term(..), For, eval)
import RecursionFormula exposing (..)

type Table
    = D1
        { n : Int
        , t : Array Int
        }
    | D2
        { h : Int
        , w : Int
        , t : Array (Array Int)
        }


initTable : Int -> ( Maybe Int ) -> Table
initTable n1 n2_ =
    case n2_ of
        -- D1
        Nothing ->
            makeTableD1 n1 <| Array.initialize n1 (\_ -> 0)
        -- D2
        Just n2 ->
            makeTableD2 n1 n2 <| Array.initialize n1 (\_ -> Array.initialize n2 (\_ -> 0))


initSizeOf : Table -> Table
initSizeOf table =
    case table of
        D1 tbl ->
            initTable tbl.n Nothing
        D2 tbl ->
            initTable tbl.h (Just tbl.w)


makeTableD1 : Int -> Array Int -> Table
makeTableD1 n t =
    D1
        { n = n
        , t = t
        }


makeTableD2 : Int -> Int -> Array (Array Int) -> Table
makeTableD2 h w t =
    D2
        { h = h
        , w = w
        , t = t
        }


dimOf : Table -> Int
dimOf table =
    case table of
        D1 _ -> 1
        D2 _ -> 2


updateSize : Int -> Maybe Int -> Table -> Table
updateSize n1 n2_ table =
    case ( n2_, table ) of
        ( Just n2, D2 tbl ) ->
            Array.initialize n1 (\i ->
                Array.initialize n2 (\j ->
                    Array.get i tbl.t
                    |> Maybe.andThen (Array.get j)
                    |> Maybe.withDefault 0
                )
            )
            |> makeTableD2 n1 n2
        ( Nothing, D1 tbl ) ->
            Array.initialize n1 (\i ->
                Array.get i tbl.t
                |> Maybe.withDefault 0
            )
            |> makeTableD1 n1
        _ -> table


editTable : Int -> Maybe Int -> Int -> Table -> Table
editTable i1 i2_ x table =
    case ( i2_, table ) of
        ( Just i2, D2 tbl ) ->
            Array.get i1 tbl.t
            |> Maybe.andThen (\row -> D2 { tbl | t = Array.set i1 ( Array.set i2 x row ) tbl.t } |> Just)
            |> Maybe.withDefault table
        ( Nothing, D1 tbl ) ->
            D1 { tbl | t = Array.set i1 x tbl.t }
        _ ->
            table


valueOf : Table -> Value
valueOf table =
    case table of
        D1 tbl ->
            Arr1 tbl.t
        D2 tbl ->
            Arr2 tbl.t


makeForCombinations : Dict Variable Value -> List For -> Maybe (List (List (Variable, Int)))
makeForCombinations dict fors =
    case fors of
        [] -> Just [[]]
        for :: rest ->
            eval dict for.begin |> Maybe.andThen (\begin ->
            eval dict for.end |> Maybe.andThen (\end ->
                List.range begin (end-1)
                |> List.map ( \n ->
                        makeForCombinations ( Dict.insert for.var (Num n) dict ) rest
                        |> Maybe.andThen (List.map ( (::) (for.var, n) ) >> Just)
                    )
                |> List.foldr
                    (\cmb acc ->
                        cmb |> Maybe.andThen (\c ->
                        acc |> Maybe.andThen (\a ->
                            c :: a |> Just))
                    )
                    (Just [])
                |> Maybe.andThen (List.concat >> Just)
            ))


valueListOf : Table -> List ( String, Value )
valueListOf table =
    case table of
        D1 tbl ->
            [("N", Num tbl.n)]
        D2 tbl ->
            [("H", Num tbl.h), ("W", Num tbl.w)]
        

applyFF : Formula -> Table -> Table
applyFF f tbl =
    case f of
        Fixed frm ->
            makeForCombinations ( valueListOf >> Dict.fromList <| tbl ) (Array.toList frm.for)
            |> Maybe.andThen
                ( List.foldl
                    (\idx acc ->
                        acc |> Maybe.andThen (\accTbl ->
                            let
                                dict =
                                    List.map (\(v, n) -> (v, Num n)) idx
                                    |> (::) ("dp", valueOf accTbl)
                                    |> Dict.fromList
                            in
                            eval dict frm.arg1 |> Maybe.andThen (\arg1 ->
                            eval dict frm.body |> Maybe.andThen (\val ->
                                case frm.arg2 of
                                    Nothing ->
                                        editTable arg1 Nothing val accTbl |> Just
                                    Just a2 ->
                                        eval dict a2 |> Maybe.andThen (\arg2 ->
                                            editTable arg1 (Just arg2) val accTbl |> Just
                                        )
                            ))
                        )
                    )
                    (Just tbl)
                )
            |> Maybe.withDefault tbl
        _ -> tbl


applyFormulas : Bool -> Array Formula -> Table -> Table
applyFormulas isInit frms tbl =
    ( if isInit then Array.foldr else Array.foldl )
        applyFF tbl frms


apply : RecursionFormulas -> Table -> Table
apply rf tbl =
    initSizeOf tbl
    |> applyFormulas True rf.init
    |> applyFormulas False rf.recursion