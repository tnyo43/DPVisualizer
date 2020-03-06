module DPTable exposing (..)

import Array exposing (Array)
import Expr exposing (Term(..))
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
        ( vw, idxW ) =
            case frm.arg2 of
                Var vw_ ->
                    ( vw_, List.repeat tbl.w vw_ |> List.indexedMap (\i v -> (v, i)) )
                Con w ->
                    ( "_", List.singleton ("_", w) )
                _ -> ( "_", [] )
        idx =
            case frm.arg1 of
                Var vh ->
                    if vh == vw
                    then List.repeat ( min tbl.h tbl.w ) vh |> List.indexedMap (\i v -> ( (v, i), (v, i) ))
                    else List.indexedMap (\h row -> List.repeat tbl.h row |> List.indexedMap (\i c -> ( (vh, i), c ))) idxW |> List.concat
                Con h ->
                    List.map (\tup -> ( ("_", h), tup )) idxW
                _ -> []
        val =
            case frm.term of
                Con x -> x
                _ -> 0 -- TODO: implement not constant case
    in
    List.foldl (\((_, h), (_, w)) accTbl -> editTable h w val accTbl) tbl idx
            