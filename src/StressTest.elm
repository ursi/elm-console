module StressTest exposing (..)

import Console exposing (basic, log)


main =
    basic
        { init = ( 0, log "1" )
        , process =
            \_ model ->
                ( model + 1, log <| String.fromInt <| model + 2 )
        }
