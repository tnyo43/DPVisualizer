port module Main exposing (main)

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
                { title = "My Elm App"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }



-- MODEL

type alias Model = String


init : () -> (Model, Cmd Msg)
init _ =
    ( "Hello Elm!"
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

view : Model -> Html Msg
view model =
    text model