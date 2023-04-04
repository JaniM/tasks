module Reactive.List exposing (RList, Stepper, empty, filter, fromDict, step, withPostStep)

import Dict
import Reactive.Dict exposing (RDict, Step(..))


type alias Stepper v =
    Step v -> List v -> Step v


type alias RList k v =
    { data : List v
    , key : v -> k
    , stepper : Stepper v
    , postStep : List v -> List v
    }


empty : (v -> k) -> RList k v
empty key =
    { data = []
    , key = key
    , stepper = always
    , postStep = identity
    }


withPostStep : (List v -> List v) -> RList k v -> RList k v
withPostStep post list =
    { list
        | data = post list.data
        , postStep = post
    }


step : Step v -> RList k v -> ( RList k v, Step v )
step s1 list =
    case list.stepper s1 list.data of
        Add x ->
            ( { list | data = list.postStep (x :: list.data) }
            , Add x
            )

        Update x ->
            ( { list
                | data =
                    replaceinList (\v -> list.key v == list.key x) x list.data
                        |> list.postStep
              }
            , Update x
            )

        Remove x ->
            ( { list
                | data =
                    List.filter (\v -> list.key v /= list.key x) list.data
                        |> list.postStep
              }
            , Remove x
            )

        None ->
            ( list, None )


fromDict : RDict k v -> RList k v
fromDict dict =
    { data = Dict.values dict.data
    , key = dict.key
    , stepper = always
    , postStep = identity
    }


filter : (v -> Bool) -> RList k v -> RList k v
filter pred list =
    { list
        | data =
            List.filter pred list.data
                |> list.postStep
        , stepper = filterStepper pred list.key
    }


filterStepper : (v -> Bool) -> (v -> k) -> Step v -> List v -> Step v
filterStepper pred key step_ state =
    case step_ of
        Add a ->
            if pred a then
                Add a

            else
                None

        Update a ->
            if pred a then
                if List.any (\v -> key v == key a) state then
                    Update a

                else
                    Add a

            else
                Remove a

        Remove a ->
            Remove a

        None ->
            None


replaceinList : (a -> Bool) -> a -> List a -> List a
replaceinList pred item list =
    let
        helper : List a -> List a -> List a
        helper acc ls =
            case ls of
                [] ->
                    List.reverse acc

                x :: xs ->
                    if pred x then
                        List.reverse acc ++ (item :: xs)

                    else
                        helper (x :: acc) xs
    in
    helper [] list
