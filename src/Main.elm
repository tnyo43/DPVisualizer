port module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RecursionFormula as RF


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
    , t : Array (Array Int)
    }

type alias Model =
    { table : Table
    , formulas : Array RF.RecursionFormula
    }


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
    ( Model
        (Table 5 5 (initTable 5 5))
        Array.empty
    , Cmd.none)



-- UPDATE

type Msg
    = UpdateH String
    | UpdateW String
    | AddRecursionFormula
    | UpdateRFArg Int Int String
    | UpdateRFTerm Int String
    | ApplyRecursionFormula Int
    | RemoveRecursionFormula Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateH h_str ->
            case String.toInt h_str of
                Just h_ ->
                    let
                        h = Basics.max 1 h_
                        t = updateTable h ( model.table.w ) model.table.t
                        table = { h = h, w = model.table.w, t = t }
                    in
                    ( { model | table = table }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

        UpdateW w_str ->
            case String.toInt w_str of
                Just w_ ->
                    let
                        w = Basics.max 1 w_
                        t = updateTable ( model.table.h ) w model.table.t
                        table = { h = model.table.h, w = w, t = t }
                    in
                    ( { model | table = table }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

        AddRecursionFormula ->
            let
                fs =
                    if Array.length ( Array.filter RF.isEditting model.formulas ) == 0
                    then Array.push (RF.init ()) model.formulas
                    else model.formulas
            in
            ( { model | formulas = fs }, Cmd.none )

        UpdateRFArg n i arg ->
            let
                fs =
                    case Array.get i model.formulas of
                        Just f -> Array.set i (RF.updateArg n arg f) model.formulas
                        Nothing -> model.formulas
            in
            ( { model | formulas = fs }, Cmd.none )


        UpdateRFTerm i term ->
            let
                fs =
                    case Array.get i model.formulas of
                        Just f -> Array.set i (RF.updateTerm term f) model.formulas
                        Nothing -> model.formulas
            in
            ( { model | formulas = fs }, Cmd.none )

        ApplyRecursionFormula i ->
            case Array.get i model.formulas of
                Just f ->
                    ( {model | formulas = Array.set i (RF.apply f) model.formulas }, Cmd.none )
                Nothing -> ( model, Cmd.none )

        RemoveRecursionFormula i ->
            let
                fs = Array.append (Array.slice 0 i model.formulas) (Array.slice (i+1) (Array.length model.formulas) model.formulas)
            in
            ( { model | formulas = fs }, Cmd.none )

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


showRecursionFormula : Array RF.RecursionFormula -> Html Msg
showRecursionFormula fs =
    ul
        []
        ( Array.toList fs
          |> List.indexedMap
                (\i -> \rf ->
                    let
                        divRf =
                            case rf of
                                RF.Editting f ->
                                    div []
                                        [ text "dp["
                                        , input [ onInput (UpdateRFArg 1 i) ] [ text f.arg1 ]
                                        , text "]["
                                        , input [ onInput (UpdateRFArg 2 i) ] [ text f.arg2 ]
                                        , text "] = "
                                        , input [ onInput (UpdateRFTerm i) ] [ text f.term ]
                                        ]
                                _ ->
                                    div [] [ RF.stringOf rf |> text ]
                    in
                    li []
                        [ divRf
                        , button
                            [ onClick ( ApplyRecursionFormula i ) ]
                            [ text "apply" ]
                        , button
                            [ onClick ( RemoveRecursionFormula i ) ]
                            [ text "x" ]
                        ]
                )
        )


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
                , model.table.h |> String.fromInt |> value
                ]
                []
            ]
        , div
            []
            [ text "W :"
            , input
                [ onInput UpdateW
                , type_ "number"
                , model.table.w |> String.fromInt |> value
                ]
                []
            ]
        , showTable model.table.t
        , button
            [ onClick AddRecursionFormula ]
            [ text "漸化式を追加する" ]
        , showRecursionFormula model.formulas
        ]