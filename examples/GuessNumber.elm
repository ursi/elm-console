module GuessNumber exposing (..)

import Console as C exposing (Cmd, Log(..))
import Console.Random as Random


main : Program () (C.Model Model) (C.Msg Msg)
main =
    C.advanced
        { init = init
        , process = process
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { number : Int
    , state : State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 GuessingNumber
    , Cmd.batch
        [ C.log "Try to guess my number!"
        , getNumber
        ]
    )


type State
    = GuessingNumber
    | GuessedCorrectly
    | Done



-- UPDATE


type Msg
    = RandomNumberReceived Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomNumberReceived n ->
            ( { model | number = n }, Cmd.none )


process : Log -> Model -> ( Model, Cmd Msg )
process log model =
    case log of
        In str ->
            case model.state of
                GuessingNumber ->
                    case String.toInt str of
                        Just n ->
                            if n == model.number then
                                ( { model | state = GuessedCorrectly }
                                , C.log <|
                                    "Good job! My number was "
                                        ++ str
                                        ++ ".\n"
                                        ++ "Do you want to play again? (y/n)"
                                )

                            else
                                ( model
                                , C.log <|
                                    "Sorry, that's too "
                                        ++ (if n > model.number then
                                                "high."

                                            else
                                                "low."
                                           )
                                )

                        Nothing ->
                            ( model, C.log "That wasn't a number. Try again!" )

                GuessedCorrectly ->
                    if str == "y" then
                        ( { model | state = GuessingNumber }
                        , Cmd.batch
                            [ C.log <| "Cool, try to guess my number again!"
                            , getNumber
                            ]
                        )

                    else
                        ( { model | state = Done }, C.log <| "Okay, bye!" )

                Done ->
                    ( model, Cmd.none )

        Out _ ->
            ( model, Cmd.none )


getNumber : Cmd Msg
getNumber =
    Random.generate RandomNumberReceived <| Random.int 1 100



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

