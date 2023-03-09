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


fmap2 : (a -> b) -> (c -> d -> a) -> c -> d -> b
fmap2 f g x y =
    f (g x y)


intModular : Float -> Float -> Int -> Int
intModular start step =
    fmap round (Element.modular start step)
