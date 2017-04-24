module Main exposing (..)

import Ports exposing (position, error)
import Types exposing (Position, Error)
import Html exposing (Html, div, text)


-- MODEL


type Msg
    = NewPositionMsg Position
    | NewErrorMsg Error
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( Model (Position 0.0 0.0) (Error 0 ""), Cmd.none )


type alias Model =
    { position : Position
    , error : Error
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPositionMsg position ->
            ( { model | position = position }, Cmd.none )

        NewErrorMsg error ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ position NewPositionMsg
        , error NewErrorMsg
        ]



-- VIEW


view : Model -> Html a
view model =
    div []
        [ div [] [ text (toString model.position.latitude) ]
        , div [] [ text (toString model.position.longitude) ]
        ]



-- MAIN


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, subscriptions = subscriptions, view = view }
