module Tasks.MainInput exposing
    ( Event(..)
    , Model
    , Msg(..)
    , defaultModel
    , projectSearch
    , update
    , view
    )

import Element exposing (..)
import Element.Background as Background
import Element.Input
import Html.Attributes
import Html.Events
import Json.Decode as D
import Json.Decode.Extra as D
import List.Extra as List
import Maybe.Extra as Maybe
import Set exposing (Set)
import Tasks.Input exposing (parseInput, projectPrefix, searchPrefix)
import Tasks.Style exposing (Style, paddingScale)
import Tasks.Task exposing (SearchRule, Task)
import Tasks.Utils exposing (listOfOne, mapFirst)


type alias DefaultState =
    { text : String
    , tagSuggestions : Maybe (List String)
    }


type alias EditState =
    { task : Task
    , text : String
    }


type alias Global =
    { tags : Set String
    , projects : List String
    }


type Model
    = Default DefaultState
    | Edit EditState


type Msg
    = SetText String
    | SubmitInput
    | Tabfill


type Event
    = None
    | AddTask String (List String)
    | SetSearch SearchRule
    | SetProject String
    | Edited Task


defaultModel : Model
defaultModel =
    Default defaultState


defaultState : DefaultState
defaultState =
    { text = "", tagSuggestions = Nothing }


findMatchingTags : Set String -> String -> List String
findMatchingTags tags tag =
    tags
        |> Set.filter (String.startsWith tag)
        |> Set.toList
        |> List.sort


withEvent : Event -> a -> ( a, Event )
withEvent e a =
    ( a, e )


withNoEvent : a -> ( a, Event )
withNoEvent a =
    ( a, None )


projectSearch : Model -> Maybe String
projectSearch model =
    case model of
        Default state ->
            case parseInput state.text of
                Ok (Tasks.Input.Project text) ->
                    Just text

                _ ->
                    Nothing

        _ ->
            Nothing


handleMainInput : DefaultState -> ( DefaultState, Event )
handleMainInput state =
    case parseInput state.text of
        Ok (Tasks.Input.Text text tags) ->
            ( defaultState, AddTask text tags )

        Ok (Tasks.Input.Project project) ->
            ( defaultState, SetProject project )

        Ok (Tasks.Input.Search rules) ->
            ( state, SetSearch rules )

        Err _ ->
            ( state, None )


{-| Complete the last tag in the current query.
Depenss on Model.tagSuggestions to be calculated properly.
`tags` should be the list of tags currently in the query.
Note: currently we reorder tags to appear after other content.
-}
tabfillTag : Global -> DefaultState -> List String -> String -> ( DefaultState, Event )
tabfillTag global state tags textBeforeTags =
    let
        tagMatching =
            state.tagSuggestions
                |> Maybe.andThen listOfOne

        init =
            List.init tags |> Maybe.unwrap [] identity

        newText =
            case tagMatching of
                Just newTag ->
                    String.join " " (textBeforeTags :: init ++ [ newTag ])
                        ++ " "

                Nothing ->
                    state.text
    in
    setText global newText state


findProjectsMatchingSearch : String -> List String -> List String
findProjectsMatchingSearch search projects =
    let
        lowerSearch =
            String.toLower search

        pred =
            String.toLower >> String.startsWith lowerSearch
    in
    List.filter pred projects


findCommonPrefix : List String -> Maybe String
findCommonPrefix strings =
    let
        first =
            List.head strings |> Maybe.withDefault ""
    in
    List.reverseRange (String.length first) 1
        |> List.map (\n -> String.slice 0 n first)
        |> List.find (\p -> List.all (String.startsWith p) strings)


{-| Perform tab completion for the main input field.
-}
tabfill : Global -> DefaultState -> ( DefaultState, Event )
tabfill global state =
    case parseInput state.text of
        Ok (Tasks.Input.Text text tags) ->
            if String.startsWith text projectPrefix then
                setText global projectPrefix state

            else if String.startsWith text searchPrefix then
                setText global searchPrefix state

            else
                tabfillTag global state tags text

        Ok (Tasks.Input.Project text) ->
            let
                prefix =
                    findProjectsMatchingSearch text global.projects
                        |> findCommonPrefix
                        |> Maybe.withDefault text
            in
            setText global (projectPrefix ++ prefix) state

        Ok (Tasks.Input.Search rule) ->
            tabfillTag
                global
                state
                rule.tags
                (searchPrefix ++ String.join " " rule.snippets)

        Err _ ->
            ( state, None )


setText : Global -> String -> DefaultState -> ( DefaultState, Event )
setText global s state =
    let
        tagsMatchingLast tags =
            List.last tags
                |> Maybe.map (findMatchingTags global.tags)
    in
    case parseInput s of
        Ok (Tasks.Input.Search rule) ->
            { state
                | text = s
                , tagSuggestions = tagsMatchingLast rule.tags
            }
                |> withEvent (SetSearch rule)

        Ok (Tasks.Input.Text _ tags) ->
            { state
                | text = s
                , tagSuggestions = tagsMatchingLast tags
            }
                |> withNoEvent

        _ ->
            { state
                | text = s
                , tagSuggestions = Nothing
            }
                |> withNoEvent


updateDefault : Global -> Msg -> DefaultState -> ( DefaultState, Event )
updateDefault global msg state =
    case msg of
        SetText text ->
            setText global text state

        SubmitInput ->
            handleMainInput state

        Tabfill ->
            tabfill global state


update : Global -> Msg -> Model -> ( Model, Event )
update global msg model =
    case model of
        Default state ->
            updateDefault global msg state
                |> mapFirst Default

        Edit state ->
            Debug.todo ""



-- VIEW


view : Style -> Model -> Element Msg
view style model =
    case model of
        Default state ->
            viewTaskInput style state

        Edit state ->
            Debug.todo ""


viewTaskInput : Style -> DefaultState -> Element Msg
viewTaskInput style model =
    let
        tagline tags =
            wrappedRow [ width fill, padding (paddingScale 2), spacing (paddingScale 2) ]
                (List.map text tags)

        suggestions =
            case model.tagSuggestions of
                Just [ tag ] ->
                    -- TODO: this is a hack
                    if String.contains (" " ++ tag) model.text then
                        Element.none

                    else
                        tagline [ tag ]

                Just tags ->
                    tagline tags

                Nothing ->
                    Element.none
    in
    el [ width fill, padding (paddingScale 2) ] <|
        Element.Input.text
            [ onKeys [ ( "Enter", SubmitInput ), ( "Tab", Tabfill ) ]
            , Background.color style.taskBackground
            , Element.Input.focusedOnLoad
            , Html.Attributes.id "input" |> htmlAttribute
            , below suggestions
            ]
            { onChange = SetText
            , text = model.text
            , placeholder = Just (Element.Input.placeholder [] (text "Add task"))
            , label = Element.Input.labelHidden "Add task"
            }


onKeys : List ( String, msg ) -> Attribute msg
onKeys pairs =
    let
        decodeKey ( key, m ) =
            D.when (D.field "key" D.string) ((==) key) (D.succeed ( m, True ))
    in
    List.map decodeKey pairs
        |> D.oneOf
        |> Html.Events.preventDefaultOn "keydown"
        |> htmlAttribute
