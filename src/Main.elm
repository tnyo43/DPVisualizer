port module Main exposing (main)

import Array exposing (Array)
import Browser
import DPTable as DP
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

type alias Model =
    { table : DP.Table
    , formulas : RF.RecursionForumulas
    }


init : () -> (Model, Cmd Msg)
init _ =
    ( Model ( DP.initTable 5 5 ) ( RF.init () )
    , Cmd.none)


-- UPDATE

type Msg
    = UpdateH String
    | UpdateW String
    | AddInitFormula
    | AddRecursionFormula
    | UpdateEdittingRFInit Int Int String
    | UpdateEdittingRFRecursion Int Int String
    | FixInit Int
    | FixRecursion Int
    | RemoveInit Int
    | RemoveRecursion Int
    | ApplyRecursionFormulas


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateH h_str ->
            case String.toInt h_str of
                Just h_ ->
                    let
                        h = Basics.max 1 h_
                        table = DP.updateSize h ( model.table.w ) model.table
                    in
                    ( { model | table = table }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

        UpdateW w_str ->
            case String.toInt w_str of
                Just w_ ->
                    let
                        w = Basics.max 1 w_
                        table = DP.updateSize ( model.table.h ) w model.table
                    in
                    ( { model | table = table }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

        AddInitFormula ->
            ( { model | formulas = RF.addInit model.formulas }, Cmd.none )

        AddRecursionFormula ->
            ( { model | formulas = RF.addRecursion model.formulas }, Cmd.none )

        UpdateEdittingRFInit row idx text ->
            ( { model | formulas = RF.updateInit row idx text model.formulas }, Cmd.none )

        UpdateEdittingRFRecursion row idx text ->
            ( { model | formulas = RF.updateRecursion row idx text model.formulas }, Cmd.none )

        FixInit row ->
            ( { model | formulas = RF.fixInit row model.formulas }, Cmd.none )

        FixRecursion row ->
            ( { model | formulas = RF.fixRecursion row model.formulas }, Cmd.none )

        RemoveInit row ->
            ( { model | formulas = RF.removeInit row model.formulas }, Cmd.none )

        RemoveRecursion row ->
            ( { model | formulas = RF.removeRecursion row model.formulas }, Cmd.none )

        ApplyRecursionFormulas -> ( model, Cmd.none )
            

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


showRecursionFormula : Bool -> Array RF.Formula -> Html Msg
showRecursionFormula isInit fs =
    ul
        []
        ( Array.toList fs
          |> List.indexedMap
                (\i -> \f ->
                    let
                        ( updateMsg, fixMsg, removeMsg ) =
                            if isInit
                            then ( UpdateEdittingRFInit i, FixInit i, RemoveInit i )
                            else ( UpdateEdittingRFRecursion i, FixRecursion i, RemoveRecursion i )
                        divRf =
                            case f of
                                RF.Editting ef ->
                                    div []
                                        [ text "dp["
                                        , input [ onInput ( updateMsg 0 ) ] [ text ef.arg1 ]
                                        , text "]["
                                        , input [ onInput ( updateMsg 1 ) ] [ text ef.arg2 ]
                                        , text "] = "
                                        , input [ onInput ( updateMsg 2 ) ] [ text ef.term ]
                                        ]
                                _ ->
                                    div [] [ RF.stringOfFormula f |> text ]
                    in
                    li []
                        [ divRf
                        , button
                            [ onClick fixMsg ]
                            [ text "fix" ]
                        , button
                            [ onClick removeMsg ]
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
            [ onClick ApplyRecursionFormulas ]
            [ text "apply" ]
        , h4 [] [ text "DP初期条件" ]
        , button
            [ onClick AddInitFormula ]
            [ text "add" ]
        , showRecursionFormula True model.formulas.init
        , h4 [] [ text "DP漸化式" ]
        , button
            [ onClick AddRecursionFormula ]
            [ text "add" ]
        , showRecursionFormula False model.formulas.recursion
        ]