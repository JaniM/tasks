module Tasks.Counter exposing (Counter, addMany, empty, list, removeMany)

import Dict exposing (Dict)
import Maybe.Extra as Maybe


type alias Counter k =
    Dict k Int


empty : Counter k
empty =
    Dict.empty


add : comparable -> Counter comparable -> Counter comparable
add key counter =
    Dict.update key (Maybe.unwrap 1 ((+) 1) >> Just) counter


addMany : List comparable -> Counter comparable -> Counter comparable
addMany keys counter =
    List.foldl add counter keys


remove : comparable -> Counter comparable -> Counter comparable
remove key counter =
    let
        updater : Maybe Int -> Maybe Int
        updater count =
            case count of
                Just 1 ->
                    Nothing

                Just x ->
                    Just (x - 1)

                _ ->
                    Nothing
    in
    Dict.update key updater counter


removeMany : List comparable -> Counter comparable -> Counter comparable
removeMany keys counter =
    List.foldl remove counter keys


list : Counter k -> List k
list =
    Dict.keys
