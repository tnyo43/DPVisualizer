port module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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

type alias Table =
    { h : Int
    , w : Int
    , table : Array (Array Int)
    }

type alias Model = Table


initTable : Int -> Int -> Array (Array Int)
initTable h w =
    Array.initialize h (\_ -> Array.initialize w (\_ -> 0))


updateTable : Int -> Int -> Array (Array Int) -> Array (Array Int)
updateTable h w table =
    Array.initialize h (\i ->
        Array.initialize w (\j ->
            case Array.get i table |> Maybe.andThen (Array.get j) of
                Nothing -> 0
                Just x -> x
        )
    )


init : () -> (Model, Cmd Msg)
init _ =
    ( Table 5 5 (initTable 5 5)
    , Cmd.none)



-- UPDATE

type Msg
    = UpdateH String
    | UpdateW String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateH h_str ->
            case String.toInt h_str of
                Just h_ ->
                    let
                        h = Basics.max 1 h_
                        table = updateTable h ( model.w ) model.table
                    in
                    ( { model | table = table, h = h }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )
        UpdateW w_str ->
            case String.toInt w_str of
                Just w_ ->
                    let
                        w = Basics.max 1 w_
                        table = updateTable ( model.h ) w model.table
                    in
                    ( { model | table = table, w = w }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )


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
view model =
    div
        []
        [ h1 [] [text "DP visualizer"]
        , div
            []
            [ text "H :"
            , input
                [ onInput UpdateH
                , type_ "number"
                , model.h |> String.fromInt |> value
                ]
                []
            ]
        , div
            []
            [ text "W :"
            , input
                [ onInput UpdateW
                , type_ "number"
                , model.w |> String.fromInt |> value
                ]
                []
            ]
        , showTable model.table
        ]