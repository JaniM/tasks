module Tasks.Input exposing
    ( InputDesc(..)
    , parseInput
    , projectPrefix
    , searchPrefix
    )

import Parser exposing (..)
import Set
import Tasks.Model exposing (Tag)
import Tasks.Utils exposing (choose)


type InputDesc
    = Text String (List Tag)
    | Project String
    | Search String (List Tag)


projectPrefix : String
projectPrefix =
    "project:"


searchPrefix : String
searchPrefix =
    "search:"


parseInput : String -> Result (List DeadEnd) InputDesc
parseInput =
    run <|
        oneOf
            [ succeed Project
                |. token projectPrefix
                |= rest
            , succeed (\s t -> Search (String.trim s) t)
                |. token searchPrefix
                |= (chompWhile (\c -> c /= '#') |> getChompedString)
                |= multipleTags
            , succeed Text
                |= (chompWhile (\c -> c /= '#') |> getChompedString)
                |= multipleTags
            ]


rest : Parser String
rest =
    succeed String.dropLeft
        |= getOffset
        |= getSource


tag : Parser Tag
tag =
    variable
        { start = (==) '#'
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '-'
        , reserved = Set.empty
        }


multipleTags : Parser (List Tag)
multipleTags =
    let
        step tags =
            oneOf
                [ succeed (\t -> Loop (t :: tags))
                    |= tag
                , succeed (choose (Done tags) (Loop tags) << String.isEmpty)
                    |= getChompedString spaces
                ]
    in
    loop [] step
