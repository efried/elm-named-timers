port module Main exposing (ID, Model, Msg(..), Timer, TimerStatus, main)

import Browser
import Color
import Dict exposing (Dict)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Flip exposing (flip)
import Font as Text
import Html exposing (Html)
import Html.Attributes exposing (id)
import Icons
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



-- PORTS


port speakText : String -> Cmd msg



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
    , sound : Bool
    }


type alias Model =
    { timers : Dict ID Timer
    , nextID : Int
    }


type alias ID =
    Int


newTimer : Timer
newTimer =
    Timer "" "" NotStarted False


init : () -> ( Model, Cmd Msg )
init _ =
    done { timers = Dict.fromList [ ( 0, newTimer ) ], nextID = 1 }



-- UPDATE


type Msg
    = InsertTimer
    | Tick
    | EnteredName ID String
    | EnteredSeconds ID String
    | TimerStarted ID
    | TimerStopped ID
    | TimerReset ID
    | RemoveTimer ID
    | ToggleSound ID Bool


mapTimer : Model -> ID -> (Timer -> Timer) -> Model
mapTimer model id f =
    { model | timers = model.timers |> Dict.update id (Maybe.map f) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InsertTimer ->
            done
                { model
                    | timers = model.timers |> Dict.insert model.nextID newTimer
                    , nextID = model.nextID + 1
                }

        Tick ->
            let
                updateTimer : Timer -> Timer
                updateTimer timer =
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

                processTimer : Timer -> Cmd msg
                processTimer timer =
                    case timer.status of
                        Ticking secondsRemaining ->
                            if secondsRemaining - 1 <= 0 && timer.sound then
                                speakText (timer.name ++ " timer complete")

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { model | timers = model.timers |> Dict.map (\_ -> updateTimer) }
            , Cmd.batch (Dict.values model.timers |> List.map processTimer)
            )

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

        ToggleSound id sound ->
            done
                (mapTimer model
                    id
                    (\timer ->
                        { timer | sound = sound }
                    )
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 (\_ -> Tick)



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


controlButton : Msg -> String -> Element.Element Msg
controlButton msg buttonText =
    Input.button
        [ Element.padding 6
        , Border.color Color.darkBlue
        , Border.width 2
        , Border.rounded 6
        , Background.color Color.white
        , Element.mouseDown [ Font.color Color.mediumBlue ]
        ]
        { onPress = Just msg
        , label = Element.text buttonText
        }


viewControls : ID -> Timer -> Element.Element Msg
viewControls id { seconds, status } =
    case status of
        NotStarted ->
            case String.toInt seconds of
                Nothing ->
                    Element.text ""

                Just _ ->
                    controlButton (TimerStarted id) "Start"

        Ticking _ ->
            controlButton (TimerStopped id) "Pause"

        Stopped _ ->
            controlButton (TimerStarted id) "Resume"

        _ ->
            Element.none


viewResetButton : ID -> Timer -> Element.Element Msg
viewResetButton id { status } =
    case status of
        NotStarted ->
            Element.none

        _ ->
            controlButton (TimerReset id) "Reset"


viewToggleSoundButton : ID -> Timer -> Element.Element Msg
viewToggleSoundButton id { sound } =
    Input.checkbox [ Font.color Color.white ]
        { onChange = ToggleSound id
        , icon =
            \soundEnabled ->
                if soundEnabled then
                    Icons.bell

                else
                    Icons.bellOff
        , checked = sound
        , label =
            Input.labelHidden "Alert when done"
        }


viewRemoveTimerButton : ID -> Element.Element Msg
viewRemoveTimerButton id =
    Element.el
        [ Element.alignRight
        ]
        (Input.button
            [ Element.padding 6
            , Element.width <| Element.px 36
            , Element.height Element.fill
            , Font.center
            , Element.mouseDown
                [ Font.color Color.red
                ]
            , Element.mouseOver
                [ Font.color Color.red
                ]
            , Region.description "Delete Timer"
            ]
            { onPress = RemoveTimer id |> Just
            , label = Element.text "✕"
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


viewStatus : Timer -> Element.Element Msg
viewStatus timer =
    Element.el
        [ Font.size <|
            if timer.status == Complete then
                Text.large

            else
                Text.xlarge
        , Font.color Color.white
        , Element.width Element.fill
        ]
    <|
        Element.text <|
            timerStatusString <|
                timer


viewTimers : Dict ID Timer -> Element.Element Msg
viewTimers timers =
    timers
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map viewTimer
        |> Element.column
            [ Element.width Element.fill
            , Element.paddingXY 0 20
            , Element.spacingXY 0 20
            ]


viewTimer : ( ID, Timer ) -> Element.Element Msg
viewTimer ( id, timer ) =
    Element.column
        [ Background.color <|
            if timer.status == Complete then
                Color.green

            else
                Color.gray
        , Border.rounded 20
        , Element.centerX
        , Element.alignTop
        , Element.width <| Element.px 800
        , Element.height <| Element.px 200
        , Element.padding 20
        ]
        [ Element.row
            [ Element.spacing 4
            , Element.width Element.fill
            , Element.height <| Element.px 36
            ]
            [ Input.text
                [ Element.alignLeft
                , Element.width <| Element.maximum 200 Element.fill
                , Element.padding 6
                , Border.rounded 6
                , Border.color Color.darkBlue
                , Border.width 2
                ]
                { onChange = EnteredName id
                , text = timer.name
                , placeholder = Element.text "Name" |> Input.placeholder [] |> Just
                , label = Input.labelHidden "Timer Name"
                }
            , Input.text
                [ Element.alignLeft
                , Element.width <| Element.maximum 200 Element.fill
                , Element.padding 6
                , Border.rounded 6
                , Border.color Color.darkBlue
                , Border.width 2
                ]
                { onChange = EnteredSeconds id
                , text = timer.seconds
                , placeholder = Element.text "Seconds" |> Input.placeholder [] |> Just
                , label = Input.labelHidden "Timer Seconds"
                }
            , viewControls id timer
            , viewResetButton id timer
            , viewToggleSoundButton id timer
            , viewRemoveTimerButton id
            ]
        , Element.row
            [ Element.height Element.fill
            , Element.alignLeft
            , Element.centerY
            ]
            [ viewStatus timer ]
        ]


view : Model -> Html Msg
view { timers } =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color Color.darkBlue
        , Font.color Color.darkBlue
        ]
    <|
        Element.column [ Element.width Element.fill ]
            [ viewTimers timers
            , Element.el
                [ Element.centerX
                , Element.paddingEach { bottom = 20, left = 0, right = 0, top = 0 }
                ]
                (Input.button
                    [ Element.padding 6
                    , Element.width <| Element.px 36
                    , Element.height Element.fill
                    , Element.centerY
                    , Font.center
                    , Font.color Color.green
                    , Font.size Text.large
                    , Element.mouseDown
                        [ Font.color Color.white
                        ]
                    , Element.mouseOver
                        [ Font.color Color.white
                        ]
                    , Region.description "Insert Timer"
                    ]
                    { onPress = InsertTimer |> Just
                    , label = 0x2B |> Char.fromCode |> String.fromChar |> Element.text
                    }
                )
            ]
