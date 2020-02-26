module Console.Random exposing (generate, int)

import Console.Internal exposing (Cmd, Msg(..))
import Random exposing (Generator)


generate : (a -> msg) -> Generator a -> Cmd log msg
generate toMsg generator =
    Random.generate (UpdateMsg << toMsg) generator


int : Int -> Int -> Generator Int
int =
    Random.int
