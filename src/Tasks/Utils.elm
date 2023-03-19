module Tasks.Utils exposing
    ( choose
    , epoch
    , findCommonPrefix
    , findMatchingPrefix
    , groupByKey
    , intModular
    , listOfOne
    , mapFirst
    )

import Element
import List.Extra as List
import Time


choose : a -> a -> Bool -> a
choose true false bool =
    if bool then
        true

    else
        false


intModular : Float -> Float -> Int -> Int
intModular start step =
    round << Element.modular start step


epoch : Time.Posix
epoch =
    Time.millisToPosix 0


groupByKey : (a -> key) -> (key -> key -> Bool) -> List a -> List ( key, List a )
groupByKey keyFn cmp list =
    list
        |> List.groupWhile (\a b -> cmp (keyFn a) (keyFn b))
        |> List.map (\( key, group ) -> ( keyFn key, key :: group ))


{-| Check if the list has one item. If so, return it. Otherwise, return Nothing.
-}
listOfOne : List k -> Maybe k
listOfOne list =
    case list of
        [ x ] ->
            Just x

        _ ->
            Nothing


mapFirst : (a -> c) -> ( a, b ) -> ( c, b )
mapFirst f ( a, b ) =
    ( f a, b )


findMatchingPrefix : String -> List String -> List String
findMatchingPrefix search projects =
    let
        lowerSearch : String
        lowerSearch =
            String.toLower search

        pred : String -> Bool
        pred =
            String.toLower >> String.startsWith lowerSearch
    in
    List.filter pred projects


findCommonPrefix : List String -> Maybe String
findCommonPrefix strings =
    let
        first : String
        first =
            List.head strings |> Maybe.withDefault ""
    in
    List.reverseRange (String.length first) 1
        |> List.map (\n -> String.slice 0 n first)
        |> List.find (\p -> List.all (String.startsWith p) strings)
