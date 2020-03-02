module FizzBuzz exposing (..)

import Console as C exposing (Cmd, Console, Log(..))


main : Program () (Console Model) (C.Msg ())
main =
    C.basic
        { init = init
        , process = process
        }


type alias Model =
    { current : Int
    , total : Maybe Int
    }


init : ( Model, Cmd () )
init =
    ( Model 0 Nothing
    , C.log "How many numbers should I play FizzBuzz with?"
    )


process : Log -> Model -> ( Model, Cmd () )
process log model =
    case log of
        In str ->
            case model.total of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    case String.toInt str of
                        Just n ->
                            ( { model | total = Just n }
                            , C.log <| fizzBuzz 1
                            )

                        Nothing ->
                            ( model, C.log "That wasn't an integer. Try again." )

        Out _ ->
            case model.total of
                Just total ->
                    let
                        newCurrent =
                            model.current + 1
                    in
                    if newCurrent == total then
                        ( model, Cmd.none )

                    else
                        ( { model | current = newCurrent }
                        , C.log <| fizzBuzz <| newCurrent + 1
                        )

                Nothing ->
                    ( model, Cmd.none )


fizzBuzz : Int -> String
fizzBuzz n =
    let
        three =
            modBy 3 n == 0

        five =
            modBy 5 n == 0
    in
    if three && five then
        "FizzBuzz"

    else if three then
        "Fizz"

    else if five then
        "Buzz"

    else
        String.fromInt n
