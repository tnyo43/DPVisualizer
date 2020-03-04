port module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)


-- MAIN

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "tno43 DP visualize"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }



-- MODEL

type alias Model =
    { size : (Int, Int)
    , table : Array (Array Int)
    }

initTable : (Int, Int) -> Array (Array Int)
initTable size =
    Array.initialize (Tuple.first size) (\_ -> Array.initialize (Tuple.second size) (\_ -> 0))

init : () -> (Model, Cmd Msg)
init _ =
    ( Model
        (5, 5) <| initTable (5, 5)
    , Cmd.none)



-- UPDATE

type Msg
    = Hoge


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Hoge ->
            (model, Cmd.none)



-- VIEW

showRow : Array Int -> Html Msg
showRow row =
    Array.toList row
    |> List.map (\n -> td [] [ String.fromInt n |> text ])
    |> tr []

showTable : Array (Array Int) -> Html Msg
showTable tbl =
    Array.toList tbl
    |> List.map showRow
    |> table []

view : Model -> Html Msg
view model = showTable model.table