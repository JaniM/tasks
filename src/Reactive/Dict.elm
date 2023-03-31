module Reactive.Dict exposing (RDict, Step(..), Stepper, add, empty, fromNormal, remove, update)

import Dict
import List exposing (foldl)


type Step a
    = Add a
    | Update a
    | Remove a
    | None


type alias Stepper k v =
    Step v -> Dict.Dict k v -> Step v


type alias RDict k v =
    { data : Dict.Dict k v
    , key : v -> k
    , stepper : Stepper k v
    }


empty : (v -> k) -> Stepper k v -> RDict k v
empty key stepper =
    { data = Dict.empty
    , key = key
    , stepper = stepper
    }


fromNormal : Dict.Dict comparable v -> (v -> comparable) -> Stepper comparable v -> RDict comparable v
fromNormal data key stepper =
    foldl (\x d -> step (Add x) d |> Tuple.first) (empty key stepper) (Dict.values data)


step : Step v -> RDict comparable v -> ( RDict comparable v, Step v )
step s1 dict =
    case dict.stepper s1 dict.data of
        Add x ->
            ( { dict | data = Dict.insert (dict.key x) x dict.data }
            , Add x
            )

        Update x ->
            ( { dict | data = Dict.insert (dict.key x) x dict.data }
            , Update x
            )

        Remove x ->
            ( { dict | data = Dict.remove (dict.key x) dict.data }
            , Remove x
            )

        None ->
            ( dict, None )


add : v -> RDict comparable v -> ( RDict comparable v, Step v )
add =
    step << Add


update : v -> RDict comparable v -> ( RDict comparable v, Step v )
update =
    step << Update


remove : v -> RDict comparable v -> ( RDict comparable v, Step v )
remove =
    step << Remove
