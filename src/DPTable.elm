module DPTable exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Expr exposing (Variable, Value(..), Term(..), For, eval)
import RecursionFormula exposing (..)

type alias Table =
    { h : Int
    , w : Int
    , t : Array (Array Int)
    }


initTable : Int -> Int -> Table
initTable h w =
    Table h w <| Array.initialize h (\_ -> Array.initialize w (\_ -> 0))


updateSize : Int -> Int -> Table -> Table
updateSize h w table =
    let
        t =
            Array.initialize h (\i ->
                Array.initialize w (\j ->
                    case Array.get i table.t |> Maybe.andThen (Array.get j) of
                        Nothing -> 0
                        Just x -> x
                )
            )
    in
    { table | h = h, w = w, t = t }


editTable : Int -> Int -> Int -> Table -> Table
editTable r c n tbl =
    case Array.get r tbl.t of
        Just row ->
            { tbl | t = Array.set r ( Array.set c n row ) tbl.t }
        Nothing ->
            tbl


makeForCombinations : Dict Variable Value -> List For -> Maybe (List (List (Variable, Int)))
makeForCombinations dict fors =
    case fors of
        [] -> Just [[]]
        for :: rest ->
            eval dict for.begin |> Maybe.andThen (\begin ->
            eval dict for.end |> Maybe.andThen (\end ->
                let
                    comb =
                        List.range begin (end-1)
                        |> List.map ( \n ->
                                makeForCombinations ( Dict.insert for.var (Num n) dict ) rest
                                |> (Maybe.andThen (List.map ( (::) (for.var, n) ) >> Just) )
                            )
                in
                List.foldr
                    (\cmb acc ->
                        cmb |> Maybe.andThen (\c ->
                        acc |> Maybe.andThen (\a ->
                            c :: a |> Just))
                    )
                    (Just [])
                    comb
                |> Maybe.andThen (List.concat >> Just)
            ))

-- TODO : add dp table into dict
applyFF : FFixed -> Table -> Table
applyFF frm tbl =
    makeForCombinations ( Dict.fromList [("H", Num tbl.h), ("W", Num tbl.w)] ) (Array.toList frm.for)
    |> Maybe.andThen
        ( List.foldl
            (\idx acc ->
                acc |> Maybe.andThen (\accTbl ->
                    let
                        dict =
                            List.map (\(v, n) -> (v, Num n)) idx
                            |> (::) ("dp", Arr2 accTbl.t)
                            |> Dict.fromList
                    in
                    eval dict frm.arg1 |> Maybe.andThen (\arg1 ->
                    eval dict frm.arg2 |> Maybe.andThen (\arg2 ->
                    eval dict frm.body |> Maybe.andThen (\val ->
                        editTable arg1 arg2 val accTbl |> Just
                    )))
                )
            )
            (Just tbl)
        )
    |> Maybe.withDefault tbl


applyFormulas : Bool -> Array Formula -> Table -> Table
applyFormulas isInit frms tbl =
    ( if isInit then Array.foldr else Array.foldl )
        (\f ->
            case f of
                Fixed ff -> applyFF ff
                _ -> \x -> x
        )
        tbl frms


apply : RecursionFormulas -> Table -> Table
apply rf tbl =
    initTable tbl.h tbl.w
    |> applyFormulas True rf.init
    |> applyFormulas False rf.recursion