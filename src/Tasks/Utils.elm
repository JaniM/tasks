module Tasks.Utils exposing (..)

import Element
import List.Extra as List
import Time


choose : a -> a -> Bool -> a
choose true false bool =
    if bool then
        true

    else
        false


fmap : (a -> b) -> (c -> a) -> c -> b
fmap f g x =
    f (g x)


intModular : Float -> Float -> Int -> Int
intModular start step =
    fmap round (Element.modular start step)


epoch : Time.Posix
epoch =
    Time.millisToPosix 0


groupByKey : (a -> key) -> (key -> key -> Bool) -> List a -> List ( key, List a )
groupByKey keyFn cmp list =
    list
        |> List.groupWhile (\a b -> cmp (keyFn a) (keyFn b))
        |> List.map (\( key, group ) -> ( keyFn key, key :: group ))
