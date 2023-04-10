module Tasks.Input exposing
    ( InputDesc(..)
    , helpPrefix
    , parseInput
    , projectPrefix
    , searchPrefix
    )

import Parser as P exposing ((|.), (|=), DeadEnd, Parser)
import Set
import Tasks.Task exposing (Priority(..), SearchRule)
import Tasks.Utils exposing (choose)


type InputDesc
    = Text String (List String) Priority
    | Project String
    | Search SearchRule
    | Help


type TextPart
    = TextPart String
    | TagPart String
    | PriorityPart Priority


projectPrefix : String
projectPrefix =
    "/project "


searchPrefix : String
searchPrefix =
    "/search "


helpPrefix : String
helpPrefix =
    "/help"


parseInput : String -> Result (List DeadEnd) InputDesc
parseInput =
    P.run <|
        P.oneOf
            [ P.succeed Project
                |. P.token projectPrefix
                |= rest
            , P.succeed Help
                |. P.token helpPrefix
            , P.succeed partsToSearch
                |. P.token searchPrefix
                |= multipleParts searchParts
            , P.succeed partsToText
                |= multipleParts textParts
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


escapedWord : Parser String
escapedWord =
    P.succeed identity
        |. P.token "\\"
        |= word


searchParts : List (Parser TextPart)
searchParts =
    [ P.map TagPart tag
    , P.map TextPart escapedWord
    , P.map TextPart word
    ]


textParts : List (Parser TextPart)
textParts =
    [ P.map TagPart tag
    , P.map PriorityPart priorityParser
    , P.map TextPart escapedWord
    , P.map TextPart word
    ]


priorityParser : Parser Priority
priorityParser =
    P.oneOf
        [ P.succeed Low
            |. P.token "--"
        , P.succeed High
            |. P.token "++"
        ]


multipleParts : List (Parser TextPart) -> Parser (List TextPart)
multipleParts partParsers =
    let
        step : List TextPart -> Parser (P.Step (List TextPart) (List TextPart))
        step revParts =
            let
                part : Parser TextPart -> Parser (P.Step (List TextPart) b)
                part =
                    P.map (\p -> P.Loop (p :: revParts))
            in
            P.oneOf <|
                List.map part partParsers
                    ++ [ P.succeed (choose (P.Done revParts) (P.Loop revParts) << String.isEmpty)
                            |= P.getChompedString P.spaces
                       ]
    in
    P.loop [] step


partsToText : List TextPart -> InputDesc
partsToText =
    collectParts
        >> (\{ snippets, tags, priority } ->
                Text (String.join " " snippets) tags priority
           )


partsToSearch : List TextPart -> InputDesc
partsToSearch =
    collectParts
        >> (\{ snippets, tags } ->
                Search { snippets = snippets, tags = tags }
           )


type alias CollectedParts =
    { snippets : List String
    , tags : List String
    , priority : Priority
    }


collectParts : List TextPart -> CollectedParts
collectParts =
    let
        loop : List String -> List String -> Priority -> List TextPart -> CollectedParts
        loop text tags prio parts =
            case parts of
                (TextPart t) :: nparts ->
                    loop (t :: text) tags prio nparts

                (TagPart t) :: nparts ->
                    loop text (t :: tags) prio nparts

                (PriorityPart p) :: nparts ->
                    loop text tags p nparts

                [] ->
                    { snippets = text
                    , tags = tags
                    , priority = prio
                    }
    in
    loop [] [] Medium
