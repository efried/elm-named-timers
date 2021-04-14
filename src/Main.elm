module Main exposing (Model, Msg(..), main)

import Browser
import Dict exposing (Dict)
import Flip exposing (flip)
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (class, classList, placeholder, title, value)
import Html.Events exposing (onClick, onInput)
import Time
import Util exposing (done)



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
    { timers : Dict ID Timer
    , nextID : Int
    }


type alias ID =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    done { timers = Dict.fromList [ ( 0, Timer "" "" NotStarted ) ], nextID = 1 }



-- UPDATE


type Msg
    = InsertTimer
    | Tick Time.Posix
    | EnteredName ID String
    | EnteredSeconds ID String
    | TimerStarted ID
    | TimerStopped ID
    | TimerReset ID
    | RemoveTimer ID


mapTimer : Model -> ID -> (Timer -> Timer) -> Model
mapTimer model id f =
    { model | timers = model.timers |> Dict.update id (Maybe.map f) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InsertTimer ->
            done
                { model
                    | timers = model.timers |> Dict.insert model.nextID (Timer "" "" NotStarted)
                    , nextID = model.nextID + 1
                }

        Tick _ ->
            let
                updateTimer =
                    \timer ->
                        case timer.status of
                            Ticking secondsRemaining ->
                                if secondsRemaining - 1 <= 0 then
                                    { timer | status = Complete }

                                else
                                    { timer | status = Ticking (secondsRemaining - 1) }

                            NotStarted ->
                                timer

                            Stopped _ ->
                                timer

                            Complete ->
                                timer
            in
            done { model | timers = model.timers |> Dict.map (always updateTimer) }

        EnteredName id name ->
            done
                (mapTimer model
                    id
                    (\timer ->
                        { timer | name = name }
                    )
                )

        EnteredSeconds id seconds ->
            done
                (mapTimer model
                    id
                    (\timer ->
                        { timer | seconds = seconds }
                    )
                )

        TimerStarted id ->
            done
                (mapTimer model
                    id
                    (\timer ->
                        case timer.status of
                            NotStarted ->
                                case String.toInt timer.seconds of
                                    Nothing ->
                                        timer

                                    Just seconds ->
                                        { timer | status = Ticking seconds }

                            Stopped seconds ->
                                { timer | status = Ticking seconds }

                            _ ->
                                timer
                    )
                )

        TimerStopped id ->
            done
                (mapTimer model
                    id
                    (\timer ->
                        case timer.status of
                            Ticking secondsRemaining ->
                                { timer | status = Stopped secondsRemaining }

                            _ ->
                                timer
                    )
                )

        TimerReset id ->
            done
                (mapTimer model
                    id
                    (\timer ->
                        { timer | status = NotStarted }
                    )
                )

        RemoveTimer id ->
            done { model | timers = model.timers |> Dict.remove id }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


type alias TimeRemaining =
    { hours : Int
    , minutes : Int
    , seconds : Int
    }


parseRemaining : Int -> TimeRemaining
parseRemaining secondsRemaining =
    { hours =
        toFloat secondsRemaining / (60 * 3600) |> floor
    , minutes =
        toFloat (modBy 3600 secondsRemaining) / 60 |> floor
    , seconds =
        modBy 60 secondsRemaining
    }


toString : TimeRemaining -> String
toString timeRemaining =
    String.join ":"
        [ String.fromInt timeRemaining.hours |> String.padLeft 2 '0'
        , String.fromInt timeRemaining.minutes |> String.padLeft 2 '0'
        , String.fromInt timeRemaining.seconds |> String.padLeft 2 '0'
        ]


formatTimerName : String -> String
formatTimerName name =
    if String.length name > 15 then
        name
            |> String.left 15
            |> flip String.append "..."

    else
        name


timerStatusString : Timer -> String
timerStatusString { status, name } =
    case status of
        NotStarted ->
            ""

        Ticking secondsRemaining ->
            secondsRemaining |> parseRemaining |> toString

        Stopped secondsRemaining ->
            secondsRemaining |> parseRemaining |> toString

        Complete ->
            name |> formatTimerName |> flip String.append " Timer Complete"


viewControls : ID -> Timer -> Html Msg
viewControls id { seconds, status } =
    case status of
        NotStarted ->
            case String.toInt seconds of
                Nothing ->
                    text ""

                Just _ ->
                    button [ onClick (TimerStarted id) ] [ text "Start" ]

        Ticking _ ->
            button [ onClick (TimerStopped id) ] [ text "Pause" ]

        Stopped _ ->
            button [ onClick (TimerStarted id) ] [ text "Resume" ]

        _ ->
            text ""


viewResetButton : ID -> Timer -> Html Msg
viewResetButton id { status } =
    case status of
        NotStarted ->
            text ""

        _ ->
            button [ onClick (TimerReset id) ] [ text "Reset" ]


viewStatus : Timer -> Html Msg
viewStatus timer =
    h1
        [ classList
            [ ( "countdown-clock", True )
            , ( "countdown-complete", timer.status == Complete )
            ]
        ]
        [ text (timerStatusString timer) ]


viewTimer : ( ID, Timer ) -> Html Msg
viewTimer ( id, timer ) =
    div
        [ classList
            [ ( "timer", True )
            , ( "countdown-complete", timer.status == Complete )
            ]
        ]
        [ div [ class "form" ]
            [ input [ placeholder "Name", class "border-bottom", value timer.name, onInput (EnteredName id) ] []
            , input [ placeholder "Seconds", class "border-bottom", value timer.seconds, onInput (EnteredSeconds id) ] []
            , viewControls id timer
            , viewResetButton id timer
            ]
        , div
            [ class "delete-timer"
            , title "Delete timer"
            ]
            [ button [ class "circle", class "minus", class "small", onClick (RemoveTimer id) ] [] ]
        , viewStatus timer
        ]


view : Model -> Html Msg
view { timers } =
    div [ class "container" ]
        [ div [ class "timers" ] (timers |> Dict.toList |> List.sortBy Tuple.first |> List.map viewTimer)
        , button
            [ class "circle"
            , class "plus"
            , title "Add a timer"
            , onClick InsertTimer
            ]
            []
        ]
