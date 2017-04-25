module Main exposing (..)

import Ports exposing (position, error, fetching)
import Types exposing (Position, Error, CurrentTemp, Fetch)
import Html exposing (Html, div, text)
import Http
import Json.Decode as Decode


-- MODEL


type Temp
    = C
    | F


type Msg
    = NewPositionMsg Position
    | NewErrorMsg Error
    | FetchingMsg Fetch
    | NewTemperature (Result Http.Error Float)
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( Model
        (Position 0.0 0.0)
        (Error 0 "")
        False
        False
        True
        0.0
        C
    , Cmd.none
    )


type alias Model =
    { position : Position
    , error : Error
    , isFetching : Bool
    , hasError : Bool
    , isInitialRender : Bool
    , temperature : Float
    , currentType : Temp
    }



-- UPDATE


decodeTemperature : Decode.Decoder Float
decodeTemperature =
    Decode.at [ "currently", "temperature" ] Decode.float


getTemperature : Model -> Http.Request Float
getTemperature model =
    let
        ( lat, lng ) =
            ( toString model.position.latitude, toString model.position.longitude )
    in
        Http.get ("https://api.darksky.net/forecast/***REMOVED***/" ++ lat ++ "," ++ lng) decodeTemperature


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPositionMsg position ->
            ( { model | position = position }, Http.send NewTemperature (getTemperature { model | position = position }) )

        NewErrorMsg error ->
            ( { model | error = error, hasError = True, isFetching = False }, Cmd.none )

        FetchingMsg bool ->
            ( { model | isFetching = True, isInitialRender = False }, Cmd.none )

        NewTemperature (Ok temp) ->
            ( { model | temperature = temp, isFetching = False }, Cmd.none )

        NewTemperature (Err _) ->
            ( { model | isFetching = False }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ position NewPositionMsg
        , error NewErrorMsg
        , fetching FetchingMsg
        ]



-- VIEW


renderView : Model -> List (Html a)
renderView model =
    if model.isInitialRender || model.isFetching then
        [ div [] [ text "" ] ]
    else if model.hasError then
        [ div [] [ text model.error.message ] ]
    else
        [ div [] [ text (toString model.temperature) ] ]


view : Model -> Html a
view model =
    div []
        (renderView model)



-- MAIN


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, subscriptions = subscriptions, view = view }
