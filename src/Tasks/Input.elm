module Tasks.Input exposing (InputDesc(..), parseInput, projectPrefix)

import Parser exposing (..)


type InputDesc
    = Text String
    | Project String


projectPrefix : String
projectPrefix = "project:"

parseInput : String -> Result (List DeadEnd) InputDesc
parseInput =
    run <|
        oneOf
            [ succeed Project
                |. token projectPrefix
                |= rest
            , succeed Text |= rest
            ]


rest : Parser String
rest =
    succeed String.dropLeft
        |= getOffset
        |= getSource
