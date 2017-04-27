module Main exposing (..)

import Ports exposing (position, error, fetching)
import Types exposing (Position, Error, CurrentTemp, Fetch)
import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Http
import Jsonp
import Task exposing (Task)
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
    | ToggleCelsius
    | ToggleFahrenheit
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( Model
        (Position 0.0 0.0)
        (Error 0 "")
        False
        False
        True
        0
        0
        0
    , Cmd.none
    )


type alias Model =
    { position : Position
    , error : Error
    , isFetching : Bool
    , hasError : Bool
    , isInitialRender : Bool
    , temperature : Int
    , celsius : Int
    , fahrenheit : Int
    }



-- UPDATE


decodeTemperature : Decode.Decoder Float
decodeTemperature =
    Decode.at [ "currently", "temperature" ] Decode.float


getTemperature : Model -> Task Http.Error Float
getTemperature model =
    let
        ( lat, lng ) =
            ( toString model.position.latitude, toString model.position.longitude )
    in
        Jsonp.get decodeTemperature ("https://api.darksky.net/forecast/***REMOVED***/" ++ lat ++ "," ++ lng)


makeCelsius : Float -> Int
makeCelsius f =
    floor ((f - 32) * (5 / 9))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPositionMsg position ->
            ( { model | position = position }, Task.attempt NewTemperature (getTemperature { model | position = position }) )

        NewErrorMsg error ->
            ( { model | error = error, hasError = True, isFetching = False }, Cmd.none )

        FetchingMsg bool ->
            ( { model | isFetching = True, isInitialRender = False }, Cmd.none )

        NewTemperature (Ok temp) ->
            let
                floored =
                    floor temp
            in
                ( { model | temperature = floored, fahrenheit = floored, celsius = makeCelsius temp, isFetching = False }, Cmd.none )

        NewTemperature (Err _) ->
            ( { model | isFetching = False }, Cmd.none )

        ToggleCelsius ->
            ( { model | temperature = model.celsius }, Cmd.none )

        ToggleFahrenheit ->
            ( { model | temperature = model.fahrenheit }, Cmd.none )

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


renderView : Model -> List (Html Msg)
renderView model =
    if model.isInitialRender || model.isFetching then
        [ div [] [ text "" ] ]
    else if model.hasError then
        [ div [] [ text model.error.message ] ]
    else
        [ div [] [ text (toString model.temperature) ]
        , button [ onClick ToggleCelsius ] [ text "Give me Celsius" ]
        , button [ onClick ToggleFahrenheit ] [ text "Give me Fahrenheit" ]
        ]


view : Model -> Html Msg
view model =
    div []
        (renderView model)



-- MAIN


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, subscriptions = subscriptions, view = view }
