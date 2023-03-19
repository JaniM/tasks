module Tasks.Input exposing
    ( InputDesc(..)
    , parseInput
    , projectPrefix
    , searchPrefix
    )

import Parser exposing (..)
import Set
import Tasks.Task exposing (SearchRule)
import Tasks.Utils exposing (choose)


type InputDesc
    = Text String (List String)
    | Project String
    | Search SearchRule


type TextPart
    = TextPart String
    | TagPart String


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
            , succeed partsToSearch
                |. token searchPrefix
                |= multipleParts
            , succeed partsToText
                |= multipleParts
            ]


rest : Parser String
rest =
    succeed String.dropLeft
        |= getOffset
        |= getSource


tag : Parser String
tag =
    variable
        { start = (==) '#'
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '-'
        , reserved = Set.empty
        }


word : Parser String
word =
    variable
        { start = (/=) ' '
        , inner = (/=) ' '
        , reserved = Set.empty
        }


multipleParts : Parser (List TextPart)
multipleParts =
    let
        addPart revParts part =
            Loop (part :: revParts)

        step revParts =
            oneOf
                [ succeed (addPart revParts << TagPart)
                    |= tag
                , succeed (addPart revParts << TextPart)
                    |= word
                , succeed (choose (Done revParts) (Loop revParts) << String.isEmpty)
                    |= getChompedString spaces
                ]
    in
    loop [] step


partsToText : List TextPart -> InputDesc
partsToText =
    collectParts >> (\{ snippets, tags } -> Text (String.join " " snippets) tags)


partsToSearch : List TextPart -> InputDesc
partsToSearch =
    collectParts >> Search



-- SearchRule happens to have the right shape - this doesn't need to last


collectParts : List TextPart -> SearchRule
collectParts =
    let
        loop text tags parts =
            case parts of
                (TextPart t) :: nparts ->
                    loop (t :: text) tags nparts

                (TagPart t) :: nparts ->
                    loop text (t :: tags) nparts

                [] ->
                    { snippets = text
                    , tags = tags
                    }
    in
    loop [] []
