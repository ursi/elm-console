module Console.Task exposing
    ( Task
    , andThen
    , attempt
    , fail
    , map
    , map2
    , map3
    , map4
    , map5
    , mapError
    , onError
    , perform
    , sequence
    , succeed
    )

import Console.Internal exposing (Cmd, Msg(..))
import Task as T


type alias Task x a =
    T.Task x a


perform : (a -> msg) -> Task Never a -> Cmd log msg
perform toMsg task =
    T.perform (UpdateMsg << toMsg) task


attempt : (Result x a -> msg) -> Task x a -> Cmd log msg
attempt toMsg task =
    T.attempt (UpdateMsg << toMsg) task


andThen : (a -> Task x b) -> Task x a -> Task x b
andThen =
    T.andThen


succeed : a -> Task x a
succeed =
    T.succeed


fail : x -> Task x a
fail =
    T.fail


sequence : List (Task x a) -> Task x (List a)
sequence =
    T.sequence


map : (a -> b) -> Task x a -> Task x b
map =
    T.map


map2 : (a -> b -> result) -> Task x a -> Task x b -> Task x result
map2 =
    T.map2


map3 : (a -> b -> c -> result) -> Task x a -> Task x b -> Task x c -> Task x result
map3 =
    T.map3


map4 : (a -> b -> c -> d -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x result
map4 =
    T.map4


map5 : (a -> b -> c -> d -> e -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x result
map5 =
    T.map5


onError : (x -> Task y a) -> Task x a -> Task y a
onError =
    T.onError


mapError : (x -> y) -> Task x a -> Task y a
mapError =
    T.mapError
