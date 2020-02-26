module Console.Random exposing (Generator, generate, int)

import Console.Internal exposing (Cmd, Msg(..))
import Random as R


type alias Generator a =
    R.Generator a


generate : (a -> msg) -> Generator a -> Cmd log msg
generate toMsg generator =
    R.generate (UpdateMsg << toMsg) generator


int : Int -> Int -> Generator Int
int =
    R.int
