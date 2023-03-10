module Tasks.Utils exposing (..)

import Element


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
