port module Main exposing (Model, Msg(..), Timer, TimerStatus, main)

import Browser
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Flip exposing (flip)
import Html exposing (Html)
import Html.Attributes exposing (id, style)
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
    { uid : Int
    , seconds : String
    , name : String
    , status : TimerStatus
    , sound : Bool
    }


type alias Model =
    { timers : List Timer
    , uid : Int
    }


newTimer : Int -> Timer
newTimer id =
    { uid = id
    , seconds = ""
    , name = ""
    , status = NotStarted
    , sound = False
    }


init : () -> ( Model, Cmd Msg )
init _ =
    done { timers = [ newTimer 0 ], uid = 1 }



-- UPDATE


type Msg
    = InsertTimer
    | Tick
    | EnteredName Int String
    | EnteredSeconds Int String
    | TimerStarted Int
    | TimerStopped Int
    | TimerReset Int
    | RemoveTimer Int
    | ToggleSound Int Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InsertTimer ->
            done
                { model
                    | timers = model.timers ++ [ newTimer model.uid ]
                    , uid = model.uid + 1
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
            ( { model | timers = List.map updateTimer model.timers }
            , Cmd.batch (List.map processTimer model.timers)
            )

        EnteredName id name ->
            let
                updateName timer =
                    if timer.uid == id then
                        { timer | name = name }

                    else
                        timer
            in
            done
                { model | timers = List.map updateName model.timers }

        EnteredSeconds id seconds ->
            let
                updateSeconds timer =
                    if timer.uid == id then
                        { timer | seconds = seconds }

                    else
                        timer
            in
            done
                { model | timers = List.map updateSeconds model.timers }

        TimerStarted id ->
            let
                startTimer timer =
                    if timer.uid == id then
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

                    else
                        timer
            in
            done
                { model | timers = List.map startTimer model.timers }

        TimerStopped id ->
            let
                stopTimer timer =
                    if timer.uid == id then
                        case timer.status of
                            Ticking secondsRemaining ->
                                { timer | status = Stopped secondsRemaining }

                            _ ->
                                timer

                    else
                        timer
            in
            done
                { model | timers = List.map stopTimer model.timers }

        TimerReset id ->
            let
                resetTimer timer =
                    if timer.uid == id then
                        { timer | status = NotStarted }

                    else
                        timer
            in
            done
                { model | timers = List.map resetTimer model.timers }

        RemoveTimer id ->
            done { model | timers = List.filter (\timer -> timer.uid /= id) model.timers }

        ToggleSound id sound ->
            let
                toggleTimerSound timer =
                    if timer.uid == id then
                        { timer | sound = sound }

                    else
                        timer
            in
            done
                { model | timers = List.map toggleTimerSound model.timers }



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
        , Border.color darkBlue
        , Border.width 2
        , Border.rounded 6
        , Background.color white
        , Element.mouseDown [ Font.color mediumBlue ]
        ]
        { onPress = Just msg
        , label = Element.text buttonText
        }


viewControls : Timer -> Element.Element Msg
viewControls timer =
    case timer.status of
        NotStarted ->
            case String.toInt timer.seconds of
                Nothing ->
                    Element.text ""

                Just _ ->
                    controlButton (TimerStarted timer.uid) "Start"

        Ticking _ ->
            controlButton (TimerStopped timer.uid) "Pause"

        Stopped _ ->
            controlButton (TimerStarted timer.uid) "Resume"

        _ ->
            Element.none


viewResetButton : Timer -> Element.Element Msg
viewResetButton timer =
    case timer.status of
        NotStarted ->
            Element.none

        _ ->
            controlButton (TimerReset timer.uid) "Reset"


viewToggleSoundButton : Timer -> Element.Element Msg
viewToggleSoundButton timer =
    Input.checkbox [ Font.color white, Element.paddingXY 8 0 ]
        { onChange = ToggleSound timer.uid
        , icon =
            \soundEnabled ->
                if soundEnabled then
                    Icons.bell

                else
                    Icons.bellOff
        , checked = timer.sound
        , label =
            Input.labelHidden "Alert when done"
        }


viewRemoveTimerButton : Timer -> Element.Element Msg
viewRemoveTimerButton { uid } =
    Element.el
        [ Element.alignRight
        ]
        (Input.button
            [ Element.padding 6
            , Element.width <| Element.px 36
            , Element.height Element.fill
            , Font.center
            , Font.color white
            , Element.mouseDown
                [ Font.color red
                ]
            , Element.mouseOver
                [ Font.color red
                ]
            , Region.description "Delete Timer"
            ]
            { onPress = RemoveTimer uid |> Just
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
                32

            else
                64
        , Font.color white
        , Element.width Element.fill
        ]
    <|
        Element.text <|
            timerStatusString <|
                timer


viewTimers : List Timer -> Element.Element Msg
viewTimers timers =
    List.map viewTimer timers
        |> Element.column
            [ Element.width Element.fill
            , Element.paddingXY 0 20
            , Element.spacingXY 0 20
            ]


viewTimer : Timer -> Element.Element Msg
viewTimer timer =
    Element.column
        [ Background.gradient <|
            { angle = degrees 315
            , steps =
                if timer.status == Complete then
                    [ Element.rgb255 27 255 88, green ]

                else
                    [ black ]
            }
        , Element.htmlAttribute <|
            Html.Attributes.style "border-image" <|
                case timer.status of
                    Stopped _ ->
                        "linear-gradient(315deg, rgb(242,252,128), rgb(226,245,0)) 2"

                    _ ->
                        "linear-gradient(315deg, rgb(27,255,88), rgb(4,199,49)) 2"
        , Border.width 4
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
                , Border.color darkBlue
                , Border.width 2
                ]
                { onChange = EnteredName timer.uid
                , text = timer.name
                , placeholder = Element.text "Name" |> Input.placeholder [] |> Just
                , label = Input.labelHidden "Timer Name"
                }
            , Input.text
                [ Element.alignLeft
                , Element.width <| Element.maximum 200 Element.fill
                , Element.padding 6
                , Border.rounded 6
                , Border.color darkBlue
                , Border.width 2
                ]
                { onChange = EnteredSeconds timer.uid
                , text = timer.seconds
                , placeholder = Element.text "Seconds" |> Input.placeholder [] |> Just
                , label = Input.labelHidden "Timer Seconds"
                }
            , viewControls timer
            , viewResetButton timer
            , viewToggleSoundButton timer
            , viewRemoveTimerButton timer
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
        , Background.color black
        , Font.color darkBlue
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
                    , Font.color green
                    , Font.size 32
                    , Element.mouseDown
                        [ Font.color white
                        ]
                    , Element.mouseOver
                        [ Font.color white
                        ]
                    , Region.description "Insert Timer"
                    ]
                    { onPress = InsertTimer |> Just
                    , label = 0x2B |> Char.fromCode |> String.fromChar |> Element.text
                    }
                )
            ]



-- COLORS


darkBlue : Element.Color
darkBlue =
    Element.rgb255 1 23 47


mediumBlue : Element.Color
mediumBlue =
    Element.rgb255 21 59 80


green : Element.Color
green =
    Element.rgb255 4 199 49


gray : Element.Color
gray =
    Element.rgb255 130 138 149


white : Element.Color
white =
    Element.rgb255 255 255 255


red : Element.Color
red =
    Element.rgba255 230 15 15 0.596


black : Element.Color
black =
    Element.rgb255 0 0 0
