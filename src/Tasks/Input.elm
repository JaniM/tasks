module Tasks.Input exposing
    ( InputDesc(..)
    , parseInput
    , projectPrefix
    , searchPrefix
    )

import Parser as P exposing ((|.), (|=), DeadEnd, Parser)
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
    "/project "


searchPrefix : String
searchPrefix =
    "/search "


parseInput : String -> Result (List DeadEnd) InputDesc
parseInput =
    P.run <|
        P.oneOf
            [ P.succeed Project
                |. P.token projectPrefix
                |= rest
            , P.succeed partsToSearch
                |. P.token searchPrefix
                |= multipleParts
            , P.succeed partsToText
                |= multipleParts
            ]


rest : Parser String
rest =
    P.succeed String.dropLeft
        |= P.getOffset
        |= P.getSource


tag : Parser String
tag =
    P.variable
        { start = (==) '#'
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '-'
        , reserved = Set.empty
        }


word : Parser String
word =
    P.variable
        { start = (/=) ' '
        , inner = (/=) ' '
        , reserved = Set.empty
        }


multipleParts : Parser (List TextPart)
multipleParts =
    let
        step : List TextPart -> Parser (P.Step (List TextPart) (List TextPart))
        step revParts =
            P.oneOf
                [ P.succeed (\part -> P.Loop (TagPart part :: revParts))
                    |= tag
                , P.succeed (\part -> P.Loop (TextPart part :: revParts))
                    |= word
                , P.succeed (choose (P.Done revParts) (P.Loop revParts) << String.isEmpty)
                    |= P.getChompedString P.spaces
                ]
    in
    P.loop [] step


partsToText : List TextPart -> InputDesc
partsToText =
    collectParts >> (\{ snippets, tags } -> Text (String.join " " snippets) tags)


partsToSearch : List TextPart -> InputDesc
partsToSearch =
    collectParts >> Search


{-| SearchRule happens to have the right shape - this doesn't need to last
-}
collectParts : List TextPart -> SearchRule
collectParts =
    let
        loop : List String -> List String -> List TextPart -> SearchRule
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
