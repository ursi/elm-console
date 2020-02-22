module StressTest exposing (..)

import Console as C exposing (Cmd, Log(..))


main : Program () (C.Model Model) (C.Msg msg)
main =
    C.basic
        { init = ( 0, C.log "1" )
        , process = process
        }


type alias Model =
    Int


process : Log -> Model -> ( Model, Cmd msg )
process log model =
    case log of
        In _ ->
            ( model, Cmd.none )

        Out _ ->
            let
                newModel =
                    model + 1
            in
            ( newModel, C.log <| String.fromInt <| newModel + 1 )
