module Main exposing (Model, Msg(..), main)

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


type TimerStatus
    = NotStarted
    | Ticking Int
    | Stopped Int
    | Complete


type alias Timer =
    { inputSeconds : String
    , name : String
    , status : TimerStatus
    }


type alias Model =
    Timer


init : () -> ( Model, Cmd Msg )
init _ =
    ( Timer "" "" NotStarted
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | Name String
    | AdjustTime String
    | StartTimer
    | StopTimer
    | ResetTimer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model.status of
                NotStarted ->
                    ( model, Cmd.none )

                Ticking secondsRemaining ->
                    if secondsRemaining - 1 <= 0 then
                        ( { model | status = Complete }
                        , Cmd.none
                        )

                    else
                        ( { model | status = Ticking (secondsRemaining - 1) }
                        , Cmd.none
                        )

                Stopped _ ->
                    ( model
                    , Cmd.none
                    )

                Complete ->
                    ( model, Cmd.none )

        Name name ->
            ( { model | name = name }
            , Cmd.none
            )

        AdjustTime inputSeconds ->
            ( { model | inputSeconds = inputSeconds }
            , Cmd.none
            )

        StartTimer ->
            case model.status of
                NotStarted ->
                    case String.toInt model.inputSeconds of
                        Nothing ->
                            ( model, Cmd.none )

                        Just seconds ->
                            ( { model | inputSeconds = "", status = Ticking seconds }
                            , Cmd.none
                            )

                Stopped seconds ->
                    ( { model | status = Ticking seconds }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        StopTimer ->
            case model.status of
                Ticking secondsRemaining ->
                    ( { model | status = Stopped secondsRemaining }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ResetTimer ->
            ( { model | inputSeconds = "", status = NotStarted }
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
    case timer.status of
        NotStarted ->
            ""

        Ticking secondsRemaining ->
            parseRemaining secondsRemaining |> toString

        Stopped secondsRemaining ->
            parseRemaining secondsRemaining |> toString

        Complete ->
            "Done"


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Name", value model.name, onInput Name ] []
        , case model.status of
            NotStarted ->
                div []
                    [ button [ onClick StartTimer ] [ text "Start" ]
                    , input [ placeholder "Seconds", value model.inputSeconds, onInput AdjustTime ] []
                    ]

            Ticking _ ->
                button [ onClick StopTimer ] [ text "Stop" ]

            Stopped _ ->
                button [ onClick StartTimer ] [ text "Resume" ]

            _ ->
                text ""
        , case model.status of
            NotStarted ->
                text ""

            _ ->
                button [ onClick ResetTimer ] [ text "Reset" ]
        , h1 [] [ text (timerStatusString model) ]
        ]
