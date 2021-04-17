module Main exposing (Model, Msg(..), main)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Flip exposing (flip)
import Html exposing (Html)
import Html.Attributes exposing (id)
import Time
import UI exposing (color, size)
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


controlButton : Msg -> String -> Element Msg
controlButton msg buttonText =
    Input.button
        [ padding 6
        , Border.color color.darkBlue
        , Border.width 2
        , Border.rounded 6
        , Background.color color.white
        , mouseDown [ Font.color color.mediumBlue ]
        ]
        { onPress = Just msg
        , label = text buttonText
        }


viewControls : ID -> Timer -> Element Msg
viewControls id { seconds, status } =
    case status of
        NotStarted ->
            case String.toInt seconds of
                Nothing ->
                    text ""

                Just _ ->
                    controlButton (TimerStarted id) "Start"

        Ticking _ ->
            controlButton (TimerStopped id) "Pause"

        Stopped _ ->
            controlButton (TimerStarted id) "Resume"

        _ ->
            none


viewResetButton : ID -> Timer -> Element Msg
viewResetButton id { status } =
    case status of
        NotStarted ->
            none

        _ ->
            controlButton (TimerReset id) "Reset"


viewRemoveTimerButton : ID -> Element Msg
viewRemoveTimerButton id =
    el
        [ alignRight
        ]
        (Input.button
            [ padding 6
            , width <| px 36
            , height fill
            , Font.center
            , mouseDown
                [ Font.color color.red
                ]
            , mouseOver
                [ Font.color color.red
                ]
            , Region.description "Delete Timer"
            ]
            { onPress = RemoveTimer id |> Just
            , label = text "✕"
            }
        )


formatTimerName : String -> String
formatTimerName name =
    if String.length name > 15 then
        name
            |> String.left 15
            |> flip String.append "..."

    else
        name


toString : TimeRemaining -> String
toString timeRemaining =
    String.join ":"
        [ String.fromInt timeRemaining.hours |> String.padLeft 2 '0'
        , String.fromInt timeRemaining.minutes |> String.padLeft 2 '0'
        , String.fromInt timeRemaining.seconds |> String.padLeft 2 '0'
        ]


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


viewStatus : Timer -> Element Msg
viewStatus timer =
    el
        [ Font.size <|
            if timer.status == Complete then
                size.large

            else
                size.xlarge
        , Font.color color.white
        , width fill
        ]
    <|
        text <|
            timerStatusString <|
                timer


viewTimers : Dict ID Timer -> Element Msg
viewTimers timers =
    timers
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map viewTimer
        |> column
            [ width fill
            , paddingXY 0 20
            , spacingXY 0 20
            ]


viewTimer : ( ID, Timer ) -> Element Msg
viewTimer ( id, timer ) =
    column
        [ Background.color <|
            if timer.status == Complete then
                color.green

            else
                color.grey
        , Border.rounded 20
        , centerX
        , alignTop
        , width <| px 800
        , height <| px 200
        , padding 20
        ]
        [ row
            [ spacing 4
            , width fill
            , height <| px 36
            ]
            [ Input.text
                [ alignLeft
                , width <| maximum 200 fill
                , padding 6
                , Border.rounded 6
                , Border.color color.darkBlue
                , Border.width 2
                ]
                { onChange = EnteredName id
                , text = timer.name
                , placeholder = text "Name" |> Input.placeholder [] |> Just
                , label = Input.labelHidden "Timer Name"
                }
            , Input.text
                [ alignLeft
                , width <| maximum 200 fill
                , padding 6
                , Border.rounded 6
                , Border.color color.darkBlue
                , Border.width 2
                ]
                { onChange = EnteredSeconds id
                , text = timer.seconds
                , placeholder = text "Seconds" |> Input.placeholder [] |> Just
                , label = Input.labelHidden "Timer Seconds"
                }
            , viewControls id timer
            , viewResetButton id timer
            , viewRemoveTimerButton id
            ]
        , row
            [ height fill
            , alignLeft
            , centerY
            ]
            [ viewStatus timer ]
        ]


view : Model -> Html Msg
view { timers } =
    layout
        [ width fill
        , height fill
        , Background.color color.darkBlue
        , Font.color color.darkBlue
        ]
    <|
        column [ width fill ]
            [ viewTimers timers
            , el
                [ centerX
                , paddingEach { bottom = 20, left = 0, right = 0, top = 0 }
                ]
                (Input.button
                    [ padding 6
                    , width <| px 36
                    , height fill
                    , centerY
                    , Font.center
                    , Font.color color.green
                    , Font.size size.large
                    , mouseDown
                        [ Font.color color.white
                        ]
                    , mouseOver
                        [ Font.color color.white
                        ]
                    , Region.description "Insert Timer"
                    ]
                    { onPress = InsertTimer |> Just
                    , label = 0x2B |> Char.fromCode |> String.fromChar |> text
                    }
                )
            ]
