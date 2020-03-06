module DPTable exposing (..)

import Array exposing (Array)


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
