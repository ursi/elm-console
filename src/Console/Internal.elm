module Console.Internal exposing (Cmd, Model, Msg(..))

import Time exposing (Posix)


type alias Model log model =
    { logs : List log
    , currentInput : String
    , model : model
    , time : Int
    }


type Msg log msg
    = NewLog log
    | InputChanged String
    | UpdateMsg msg
    | RunCmd ( Maybe Posix, Cmd log msg )
    | Resize Int Int
    | NoOp


type alias Cmd log msg =
    Cmd.Cmd (Msg log msg)
