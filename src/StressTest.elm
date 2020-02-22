module StressTest exposing (..)

import Console exposing (Log(..), basic, log)


main =
    basic
        { init = ( 0, log "1" )
        , process =
            \log_ model ->
                case log_ of
                    In _ ->
                        ( model, Cmd.none )

                    Out _ ->
                        ( model + 1, log <| String.fromInt <| model + 2 )
        }
