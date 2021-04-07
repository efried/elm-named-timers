module Main exposing (Model, Msg(..), main)

import Browser
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (class, placeholder, title, value)
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
    { timers : List ( ID, Timer )
    , nextID : Int
    }


type alias ID =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { timers = List.singleton ( 0, Timer "" "" NotStarted ), nextID = 1 }
    , Cmd.none
    )



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



-- make multiple timers like this https://benbooth.dev/building-a-basic-ui-clone-of-instagram-using-elm-part-2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InsertTimer ->
            ( { model
                | timers = model.timers ++ List.singleton ( model.nextID, Timer "" "" NotStarted )
                , nextID = model.nextID + 1
              }
            , Cmd.none
            )

        Tick _ ->
            let
                updateTimer ( id, timer ) =
                    case timer.status of
                        NotStarted ->
                            ( id, timer )

                        Ticking secondsRemaining ->
                            if secondsRemaining - 1 <= 0 then
                                ( id
                                , { timer | status = Complete }
                                )

                            else
                                ( id
                                , { timer | status = Ticking (secondsRemaining - 1) }
                                )

                        Stopped _ ->
                            ( id, timer )

                        Complete ->
                            ( id, timer )
            in
            ( { model | timers = List.map updateTimer model.timers }
            , Cmd.none
            )

        EnteredName id name ->
            let
                updateTimerName ( timerID, timer ) =
                    if timerID == id then
                        ( timerID
                        , { timer | name = name }
                        )

                    else
                        ( timerID, timer )
            in
            ( { model | timers = List.map updateTimerName model.timers }
            , Cmd.none
            )

        EnteredSeconds id seconds ->
            let
                updateTimerSeconds ( timerID, timer ) =
                    if timerID == id then
                        ( timerID
                        , { timer | seconds = seconds }
                        )

                    else
                        ( timerID, timer )
            in
            ( { model | timers = List.map updateTimerSeconds model.timers }
            , Cmd.none
            )

        TimerStarted id ->
            let
                startTimer ( timerID, timer ) =
                    if timerID == id then
                        case timer.status of
                            NotStarted ->
                                case String.toInt timer.seconds of
                                    Nothing ->
                                        ( timerID, timer )

                                    Just seconds ->
                                        ( timerID
                                        , { timer | seconds = "", status = Ticking seconds }
                                        )

                            Stopped seconds ->
                                ( timerID
                                , { timer | status = Ticking seconds }
                                )

                            _ ->
                                ( timerID, timer )

                    else
                        ( timerID, timer )
            in
            ( { model | timers = List.map startTimer model.timers }
            , Cmd.none
            )

        TimerStopped id ->
            let
                stopTimer ( timerID, timer ) =
                    if timerID == id then
                        case timer.status of
                            Ticking secondsRemaining ->
                                ( timerID
                                , { timer | status = Stopped secondsRemaining }
                                )

                            _ ->
                                ( timerID, timer )

                    else
                        ( timerID, timer )
            in
            ( { model | timers = List.map stopTimer model.timers }
            , Cmd.none
            )

        TimerReset id ->
            let
                resetTimer ( timerID, timer ) =
                    if timerID == id then
                        ( timerID
                        , { timer | seconds = "", status = NotStarted }
                        )

                    else
                        ( timerID, timer )
            in
            ( { model | timers = List.map resetTimer model.timers }
            , Cmd.none
            )

        RemoveTimer id ->
            ( { model | timers = List.filter (\( timerID, _ ) -> timerID /= id) model.timers }
            , Cmd.none
            )



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
    (String.fromInt timeRemaining.hours |> String.padLeft 2 '0')
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


viewTimer : ( ID, Timer ) -> Html Msg
viewTimer ( id, timer ) =
    div
        [ class "timer"
        ]
        [ button [ onClick (RemoveTimer id) ] [ text "Delete Timer" ]
        , input [ placeholder "Name", value timer.name, onInput (EnteredName id) ] []
        , case timer.status of
            NotStarted ->
                div []
                    [ button [ onClick (TimerStarted id) ] [ text "Start" ]
                    , input [ placeholder "Seconds", value timer.seconds, onInput (EnteredSeconds id) ] []
                    ]

            Ticking _ ->
                button [ onClick (TimerStopped id) ] [ text "Stop" ]

            Stopped _ ->
                button [ onClick (TimerStarted id) ] [ text "Resume" ]

            _ ->
                text ""
        , case timer.status of
            NotStarted ->
                text ""

            _ ->
                button [ onClick (TimerReset id) ] [ text "Reset" ]
        , h1 [] [ text (timerStatusString timer) ]
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "timers" ] (List.map viewTimer model.timers)
        , button
            [ class "circle"
            , class "plus"
            , title "Add another timer"
            , onClick InsertTimer
            ]
            []
        ]
