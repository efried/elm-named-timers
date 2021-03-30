module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Basics exposing (remainderBy)
import Browser
import Html exposing (..)
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
    = NotStarted Int
    | Ticking Int
    | Complete


type alias Model =
    { timer : Timer }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { timer = NotStarted 500 }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model.timer of
                NotStarted secondsRemaining ->
                    ( { model | timer = Ticking (secondsRemaining - 1) }
                    , Cmd.none
                    )

                Ticking secondsRemaining ->
                    if secondsRemaining - 1 == 0 then
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


view : Model -> Html Msg
view model =
    case model.timer of
        NotStarted secondsRemaining ->
            h1 [] [ text (parseRemaining secondsRemaining |> toString) ]

        Ticking secondsRemaining ->
            h1 [] [ text (parseRemaining secondsRemaining |> toString) ]

        Complete ->
            h1 [] [ text "Done" ]
