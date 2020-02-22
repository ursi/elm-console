module Console.Internal exposing (..)

import Time exposing (Posix)


type Msg log msg
    = NewLog log
    | InputChanged String
    | UpdateMsg msg
    | RunCmd ( Maybe Posix, Cmd log msg )
    | Resize Int Int
    | NoOp


type alias Cmd log msg =
    Cmd.Cmd (Msg log msg)
