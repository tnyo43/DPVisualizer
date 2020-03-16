port module Main exposing (main)

import Array exposing (Array)
import Browser
import DPTable as DP
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import RecursionFormula as RF
import Styles


-- MAIN

main : Program () Model Msg
main = 
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "tno43 DP visualize"
                , body = [ view m |> toUnstyled ]
                }
        , subscriptions = \_ -> Sub.none
        }



-- MODEL

type alias Model =
    { table : DP.Table
    , formulas : RF.RecursionFormulas
    , selectedCel : ( Int, Int )
    }


init : () -> (Model, Cmd Msg)
init _ =
    ( Model
        ( DP.initTable 5 5 )
        ( RF.init () )
        ( -2, -2 ) -- dp table is -1 indexed
    , Cmd.none)


-- UPDATE

type Msg
    = UpdateH String
    | UpdateW String
    | AddInitFormula
    | AddRecursionFormula
    | UpdateEdittingRFInit Int Int String
    | UpdateEdittingRFRecursion Int Int String
    | AddInitFor Int
    | AddRecursionFor Int
    | ResetInitFor Int
    | ResetRecursionFor Int
    | UpdateInitFor Int Int Int String
    | UpdateRecursionFor Int Int Int String
    | FixInit Int
    | FixRecursion Int
    | RemoveInit Int
    | RemoveRecursion Int
    | ApplyRecursionFormulas
    | OverDPCel Int Int
    | OutDPCel


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

        AddInitFor row ->
            ( { model | formulas = RF.addInitFor row model.formulas }, Cmd.none )

        AddRecursionFor row ->
            ( { model | formulas = RF.addRecursionFor row model.formulas }, Cmd.none )

        ResetInitFor row ->
            ( { model | formulas = RF.resetInitFor row model.formulas }, Cmd.none )

        ResetRecursionFor row ->
            ( { model | formulas = RF.resetRecursionFor row model.formulas }, Cmd.none )

        UpdateInitFor row rowFor idx text ->
            ( { model | formulas = RF.updateInitFor row rowFor idx text model.formulas }, Cmd.none )

        UpdateRecursionFor row rowFor idx text ->
            ( { model | formulas = RF.updateRecursionFor row rowFor idx text model.formulas }, Cmd.none )

        FixInit row ->
            ( { model | formulas = RF.fixInit row model.formulas }, Cmd.none )

        FixRecursion row ->
            ( { model | formulas = RF.fixRecursion row model.formulas }, Cmd.none )

        RemoveInit row ->
            ( { model | formulas = RF.removeInit row model.formulas }, Cmd.none )

        RemoveRecursion row ->
            ( { model | formulas = RF.removeRecursion row model.formulas }, Cmd.none )

        ApplyRecursionFormulas ->
            let
                tbl = DP.initTable model.table.h model.table.w
            in
            ( { model | table = DP.apply model.formulas tbl }, Cmd.none )

        OverDPCel row col ->
            ( { model | selectedCel = ( row, col )}, Cmd.none )
        
        OutDPCel ->
            ( { model | selectedCel = ( -2, -2 )}, Cmd.none )

-- VIEW

showTable : DP.Table -> (Int, Int) -> Html Msg
showTable tbl slct =
    Array.toList tbl.t
    |> List.indexedMap (\row ->
            Array.toList
            >> (List.indexedMap
                    (\col n ->
                        let
                            style =
                                if Tuple.first slct == row && Tuple.second slct == col
                                then Styles.dpTableSelectedCel
                                else if Tuple.first slct == row || Tuple.second slct == col
                                then Styles.dpTableRowColSelectedCel
                                else Styles.dpTableCel
                        in
                        td
                            ( onMouseEnter ( OverDPCel row col ) :: style ) 
                            [ String.fromInt n |> text ])
                )
            >> ((::)
                    (td
                        (
                            let
                                style =
                                    if Tuple.first slct == row
                                    then Styles.dpTableRowColSelectedCel
                                    else Styles.dpTableIndex
                            in
                            onMouseEnter OutDPCel :: style
                        )
                        [ String.fromInt row |> text ] ))
            >> (tr [])
        )
    |> ((::)
            ( ( List.range -1 (tbl.w-1)
                |> (List.map
                        (\col ->
                            td
                                ( if Tuple.second slct == col
                                  then Styles.dpTableRowColSelectedCel
                                  else Styles.dpTableIndex
                                )
                                [ text (if col >= 0 then String.fromInt col else "") ]
                        )
                    )
              )
              |> (tr [ onMouseEnter OutDPCel ])
            )
        )
    |> table ( onMouseLeave OutDPCel :: Styles.dpTable )


showFormula : RF.Formula -> (Int -> String -> Msg) -> Html Msg
showFormula f updateMsg =
    case f of
        RF.Editting ef ->
            div []
                [ text "dp["
                , input [ onInput ( updateMsg 0 ) ] [ text ef.arg1 ]
                , text "]["
                , input [ onInput ( updateMsg 1 ) ] [ text ef.arg2 ]
                , text "] = "
                , input [ onInput ( updateMsg 2 ) ] [ text ef.body ]
                ]
        _ -> 
            div [] [ RF.stringOfFormula f |> text ]


showForsOfFormula : RF.Formula -> Msg -> Msg -> (Int -> Int -> String -> Msg) -> Html Msg
showForsOfFormula f addMsg resetMsg updateForMsg =
    case f of
        RF.Editting ef ->
            div []
                (
                    List.indexedMap
                        (\rowFor (var, begin, end) ->
                            div []
                                [ text "for ("
                                , input [ onInput ( updateForMsg rowFor 0 ) ] []
                                , text " = "
                                , input [ onInput ( updateForMsg rowFor 1 ) ] []
                                , text <| "; " ++ var ++ " < "
                                , input [ onInput ( updateForMsg rowFor 2 ) ] []
                                , text <| "; " ++ var ++ "++)"
                                ]
                        )
                        ( Array.toList ef.for )
                    ++
                    [ button
                        [ onClick addMsg ]
                        [ text "add for" ]
                    , button
                        [ onClick resetMsg ]
                        [ text "reset for" ]
                    ]
                )
        _ ->
            div []
                [ text <| RF.stringOfFor f ]


showRecursionFormulas : Bool -> Array RF.Formula -> Html Msg
showRecursionFormulas isInit fs =
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
                        ( addForMsg, resetForMsg, updateForMsg ) =
                            if isInit
                            then ( AddInitFor i, ResetInitFor i, UpdateInitFor i )
                            else ( AddRecursionFor i, ResetRecursionFor i, UpdateRecursionFor i )
                    in
                    li []
                        [ showFormula f updateMsg
                        , showForsOfFormula f addForMsg resetForMsg updateForMsg
                        , button
                            [ onClick fixMsg ]
                            [ text "fix" ]
                        , button
                            [ onClick removeMsg ]
                            [ text "x" ]
                        ]
                )
        )


view : Model -> Html.Styled.Html Msg
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
        , showTable model.table model.selectedCel
        , button
            [ onClick ApplyRecursionFormulas ]
            [ text "apply" ]
        , h4 [] [ text "DP初期条件" ]
        , button
            [ onClick AddInitFormula ]
            [ text "add" ]
        , showRecursionFormulas True model.formulas.init
        , h4 [] [ text "DP漸化式" ]
        , button
            [ onClick AddRecursionFormula ]
            [ text "add" ]
        , showRecursionFormulas False model.formulas.recursion
        ]