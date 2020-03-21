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
        ( DP.initTable 5 (Just 5) )
        ( RF.init () )
        ( -2, -2 ) -- dp table is -1 indexed
    , Cmd.none)


-- UPDATE

type Msg
    = SwitchDPDim
    | UpdateN String
    | UpdateH String
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
        SwitchDPDim ->
            let
                table =
                    case model.table of
                        DP.D1 tbl ->
                            DP.initTable 5 (Just 5)
                        DP.D2 tbl ->
                            DP.initTable 5 Nothing
            in
            ( { model | table = table, formulas = RF.init() }, Cmd.none )

        UpdateN n_str ->
            case ( String.toInt n_str, model.table ) of
                ( Just n_, DP.D1 tbl ) ->  
                    let
                        n = Basics.max 1 n_
                        table = DP.updateSize n Nothing model.table
                    in
                    ( { model | table = table }, Cmd.none )
                _ ->
                    ( model, Cmd.none )

        UpdateH h_str ->
            case ( String.toInt h_str, model.table ) of
                ( Just h_, DP.D2 tbl ) ->
                    let
                        h = Basics.max 1 h_
                        table = DP.updateSize h ( Just tbl.w ) model.table
                    in
                    ( { model | table = table }, Cmd.none )
                _ ->
                    ( model, Cmd.none )

        UpdateW w_str ->
            case ( String.toInt w_str, model.table ) of
                ( Just w_, DP.D2 tbl ) ->
                    let
                        w = Basics.max 1 w_
                        table = DP.updateSize tbl.h ( Just w ) model.table
                    in
                    ( { model | table = table }, Cmd.none )
                _ ->
                    ( model, Cmd.none )

        AddInitFormula ->
            ( { model | formulas = RF.addInit (DP.dimOf model.table) model.formulas }, Cmd.none )

        AddRecursionFormula ->
            ( { model | formulas = RF.addRecursion (DP.dimOf model.table) model.formulas }, Cmd.none )

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
                table =
                    case model.table of
                        DP.D1 tbl ->
                            DP.initTable tbl.n Nothing
                        DP.D2 tbl ->
                            DP.initTable tbl.h (Just tbl.w)
            in
            ( { model | table = DP.apply model.formulas table }, Cmd.none )

        OverDPCel row col ->
            ( { model | selectedCel = ( row, col )}, Cmd.none )
        
        OutDPCel ->
            ( { model | selectedCel = ( -2, -2 )}, Cmd.none )

-- VIEW

showTable : DP.Table -> (Int, Int) -> Html Msg
showTable table slct =
    case table of
        DP.D1 tbl ->
            showTableD1 (tbl.n, tbl.t) (Tuple.first slct)
        DP.D2 tbl ->
            showTableD2 (tbl.h, tbl.w, tbl.t) slct


showTableD1 : (Int, Array Int) -> Int -> Html Msg
showTableD1 (n, tbl) slct =
    Array.toList tbl
    |> List.indexedMap (\i x ->
            let 
                style =
                    if slct == i
                    then Styles.dpTableSelectedCel
                    else Styles.dpTableCel
            in
            td
                ( onMouseEnter (OverDPCel i -2 ) :: style )
                [ String.fromInt x |> text ]
        )
    |> tr []
    |> List.singleton
    |> ((::)
        ( ( List.range 0 (n-1)
            |> (List.map
                    (\i ->
                        td
                            ( if slct == i
                                then Styles.dpTableRowColSelectedCel
                                else Styles.dpTableIndex
                            )
                            [ text (if i >= 0 then String.fromInt i else "") ]
                    )
                )
            )
            |> (tr [ onMouseEnter OutDPCel ])
        )
    )
    |> table ( onMouseLeave OutDPCel :: Styles.dpTable )



showTableD2 : (Int, Int, Array (Array Int)) -> (Int, Int) -> Html Msg
showTableD2 (h, w, tbl) slct =
    Array.toList tbl
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
                            [ String.fromInt n |> text ]
                    )
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
                        [ String.fromInt row |> text ]
                    )
                )
            >> (tr [])
        )
    |> ((::)
            ( ( List.range -1 (w-1)
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
                (
                    [ text "dp["
                    , input [ onInput ( updateMsg 0 ) ] [ text ef.arg1 ]
                    ]
                    ++
                    (
                        ef.arg2
                        |> Maybe.andThen (\a2 ->
                            Just
                                [ text "]["
                                , input [ onInput ( updateMsg 1 ) ] [ text a2 ]
                                ]
                            )
                        |> Maybe.withDefault []
                    )
                    ++
                    [ text "] = "
                    , input [ onInput ( updateMsg 2 ) ] [ text ef.body ]
                    ]
                )
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
        [ h1 [] [text "DP visualizer"]
        , button
            [ onClick SwitchDPDim ]
            [ DP.dimOf model.table |> String.fromInt |> (\n -> n ++ "次元DP" ) |> text ]
        ]
        ++
        ( case model.table of
            DP.D1 tbl ->
                [ div []
                    [ text "N :"
                    , input
                        [ onInput UpdateN
                        , type_ "number"
                        , tbl.n |> String.fromInt |> value
                        ]
                        []
                    ]
                ]
            DP.D2 tbl ->
                [ div
                    []
                    [ text "H :"
                    , input
                        [ onInput UpdateH
                        , type_ "number"
                        , tbl.h |> String.fromInt |> value
                        ]
                        []
                    ]
                , div
                    []
                    [ text "W :"
                    , input
                        [ onInput UpdateW
                        , type_ "number"
                        , tbl.w |> String.fromInt |> value
                        ]
                        []
                    ]
                ]
        )
        ++
        [ showTable model.table model.selectedCel
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
        |> div []