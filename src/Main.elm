module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Basics exposing (remainderBy)
import Browser
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Timer
    = NotStarted
    | Ticking Int
    | Complete


type alias Model =
    { input : String
    , timer : Timer
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" NotStarted
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTime String
    | StartTimer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model.timer of
                NotStarted ->
                    ( model
                    , Cmd.none
                    )

                Ticking secondsRemaining ->
                    if secondsRemaining - 1 <= 0 then
                        ( { model | timer = Complete }
                        , Cmd.none
                        )

                    else
                        ( { model | timer = Ticking (secondsRemaining - 1) }
                        , Cmd.none
                        )

                Complete ->
                    ( model
                    , Cmd.none
                    )

        AdjustTime newInput ->
            ( { model | input = newInput }
            , Cmd.none
            )

        StartTimer ->
            case String.toInt model.input of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just seconds ->
                    ( { model | timer = Ticking seconds }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


type alias TimeRemaining =
    { days : Int
    , hours : Int
    , minutes : Int
    , seconds : Int
    }


parseRemaining : Int -> TimeRemaining
parseRemaining secondsRemaining =
    { days =
        floor (toFloat secondsRemaining / (24 * 3600))
    , hours =
        floor (toFloat (remainderBy (24 * 3600) secondsRemaining) / 3600)
    , minutes =
        floor (toFloat (remainderBy 3600 secondsRemaining) / 60)
    , seconds =
        remainderBy 60 secondsRemaining
    }


toString : TimeRemaining -> String
toString timeRemaining =
    "Days: "
        ++ String.fromInt timeRemaining.days
        ++ ", Hours: "
        ++ String.fromInt timeRemaining.hours
        ++ ", Minutes: "
        ++ String.fromInt timeRemaining.minutes
        ++ ", Seconds: "
        ++ String.fromInt timeRemaining.seconds


timerStatusString : Timer -> String
timerStatusString timer =
    case timer of
        NotStarted ->
            "Waiting..."

        Ticking secondsRemaining ->
            parseRemaining secondsRemaining |> toString

        Complete ->
            "Done"


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ placeholder "Seconds", value model.input, onInput AdjustTime ] []
            , button [ onClick StartTimer ] [ text "Start" ]
            ]
        , h1 [] [ text (timerStatusString model.timer) ]
        ]
