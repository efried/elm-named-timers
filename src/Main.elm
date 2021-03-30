module Main exposing (Model, Msg(..), main)

import Browser
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (placeholder, style, value)
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
    { seconds : String
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
    | EnteredName String
    | EnteredSeconds String
    | TimerStarted
    | TimerStopped
    | TimerReset


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

        EnteredName name ->
            ( { model | name = name }
            , Cmd.none
            )

        EnteredSeconds seconds ->
            ( { model | seconds = seconds }
            , Cmd.none
            )

        TimerStarted ->
            case model.status of
                NotStarted ->
                    case String.toInt model.seconds of
                        Nothing ->
                            ( model, Cmd.none )

                        Just seconds ->
                            ( { model | seconds = "", status = Ticking seconds }
                            , Cmd.none
                            )

                Stopped seconds ->
                    ( { model | status = Ticking seconds }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TimerStopped ->
            case model.status of
                Ticking secondsRemaining ->
                    ( { model | status = Stopped secondsRemaining }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TimerReset ->
            ( { model | seconds = "", status = NotStarted }
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
        toFloat secondsRemaining / (24 * 3600) |> floor
    , hours =
        toFloat (modBy (24 * 3600) secondsRemaining) / 3600 |> floor
    , minutes =
        toFloat (modBy 3600 secondsRemaining) / 60 |> floor
    , seconds =
        modBy 60 secondsRemaining
    }


toString : TimeRemaining -> String
toString timeRemaining =
    (String.fromInt timeRemaining.days |> String.padLeft 2 '0')
        ++ ":"
        ++ (String.fromInt timeRemaining.hours |> String.padLeft 2 '0')
        ++ ":"
        ++ (String.fromInt timeRemaining.minutes |> String.padLeft 2 '0')
        ++ ":"
        ++ (String.fromInt timeRemaining.seconds |> String.padLeft 2 '0')


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


viewTimer : Timer -> Html Msg
viewTimer timer =
    div
        [ style "background-color" "rgb(202,227,219)"
        , style "border" "6px dashed #ccc"
        , style "border-radius" "20px"
        , style "width" "480px"
        , style "height" "100px"
        , style "margin" "100px auto"
        , style "padding" "20px"
        ]
        [ input [ placeholder "Name", value timer.name, onInput EnteredName ] []
        , case timer.status of
            NotStarted ->
                div []
                    [ button [ onClick TimerStarted ] [ text "Start" ]
                    , input [ placeholder "Seconds", value timer.seconds, onInput EnteredSeconds ] []
                    ]

            Ticking _ ->
                button [ onClick TimerStopped ] [ text "Stop" ]

            Stopped _ ->
                button [ onClick TimerStarted ] [ text "Resume" ]

            _ ->
                text ""
        , case timer.status of
            NotStarted ->
                text ""

            _ ->
                button [ onClick TimerReset ] [ text "Reset" ]
        , h1 [] [ text (timerStatusString timer) ]
        ]


view : Model -> Html Msg
view model =
    viewTimer model
