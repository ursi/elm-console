module HelloWorld exposing (..)

import Console as C


main =
    C.basic
        { init = ( (), C.log "Hello, World!" )
        , process = \_ _ -> ( (), Cmd.none )
        }
