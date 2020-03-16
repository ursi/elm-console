module Console.Random exposing
    ( Generator
    , Seed
    , andThen
    , constant
    , float
    , generate
    , independentSeed
    , initialSeed
    , int
    , lazy
    , list
    , map
    , map2
    , map3
    , map4
    , map5
    , maxInt
    , minInt
    , pair
    , step
    , uniform
    , weighted
    )

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


float : Float -> Float -> Generator Float
float =
    R.float


uniform : a -> List a -> Generator a
uniform =
    R.uniform


weighted : ( Float, a ) -> List ( Float, a ) -> Generator a
weighted =
    R.weighted


constant : a -> Generator a
constant =
    R.constant


pair : Generator a -> Generator b -> Generator ( a, b )
pair =
    R.pair


list : Int -> Generator a -> Generator (List a)
list =
    R.list


map : (a -> b) -> Generator a -> Generator b
map =
    R.map


map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 =
    R.map2


map3 : (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
map3 =
    R.map3


map4 : (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
map4 =
    R.map4


map5 : (a -> b -> c -> d -> e -> f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
map5 =
    R.map5


andThen : (a -> Generator b) -> Generator a -> Generator b
andThen =
    R.andThen


lazy : (() -> Generator a) -> Generator a
lazy =
    R.lazy


maxInt : Int
maxInt =
    R.maxInt


minInt : Int
minInt =
    R.minInt


type alias Seed =
    R.Seed


step : Generator a -> Seed -> ( a, Seed )
step =
    R.step


initialSeed : Int -> Seed
initialSeed =
    R.initialSeed


independentSeed : Generator Seed
independentSeed =
    R.independentSeed
