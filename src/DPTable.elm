module DPTable exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Expr exposing (Term(..), eval)
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


apply : Table -> FFixed -> Table
apply tbl frm =
    let
        ( vw_, idxW ) =
            case frm.arg2 of
                Var v ->
                    ( v, List.repeat tbl.w v |> List.indexedMap (\i vr -> (vr, i)) )
                Con w ->
                    ( "_", List.singleton ("_", w) )
                _ -> ( "_", [] )
        idx =
            case frm.arg1 of
                Var vh ->
                    if vh == vw_
                    then List.repeat ( min tbl.h tbl.w ) vh |> List.indexedMap (\i v -> ( (v, i), (v, i) ))
                    else List.indexedMap (\h row -> List.repeat tbl.h row |> List.indexedMap (\i c -> ( (vh, i), c ))) idxW |> List.concat
                Con h ->
                    List.map (\tup -> ( ("_", h), tup )) idxW
                _ -> []
    in
    List.foldl
        (\((vh, h), (vw, w)) acc ->
            acc |> Maybe.andThen (\accTbl ->
            eval (Dict.fromList [(vh, h), (vw, w)]) frm.body |> Maybe.andThen (\val ->
                editTable h w val accTbl |> Just
            ))
        )
        (Just tbl) idx
    |> Maybe.withDefault tbl


applyFormulas : Table -> Array FFixed -> Table
applyFormulas tbl frms =
    Array.foldl (\f accTbl -> apply accTbl f) tbl frms
