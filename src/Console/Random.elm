module Console.Random exposing (..)

import Console.Internal exposing (Cmd, Msg(..))
import Random exposing (Generator)


todo =
    Debug.todo ""


generate : (a -> msg) -> Generator a -> Cmd log msg
generate toMsg generator =
    Random.generate (\a -> UpdateMsg <| toMsg a) generator


int =
    Random.int
