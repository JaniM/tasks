module Tasks.MainInput exposing
    ( Event(..)
    , Global
    , Model
    , Msg(..)
    , defaultModel
    , projectSearch
    , update
    , view
    )

import Element exposing (Attribute, Element, el, fill, padding, spacing, width, wrappedRow)
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
import Tasks.Task exposing (SearchRule)
import Tasks.Utils exposing (findCommonPrefix, findMatchingPrefix, listOfOne, mapFirst)


type alias DefaultState =
    { text : String
    , tagSuggestions : Maybe (List String)
    }


type alias Global =
    { tags : Set String
    , projects : List String
    , project : Maybe String
    }


type Model
    = Default DefaultState


type Msg
    = SetText String
    | SubmitInput
    | Tabfill


type Event
    = None
    | AddTask String (List String)
    | SetSearch SearchRule
    | SetProject String


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
tabfillTag : Global -> DefaultState -> List String -> List String -> ( DefaultState, Event )
tabfillTag global state tags textBeforeTags =
    let
        tagMatching : Maybe String
        tagMatching =
            state.tagSuggestions
                |> Maybe.andThen listOfOne

        newText : String
        newText =
            case tagMatching of
                Just newTag ->
                    let
                        init : List String
                        init =
                            List.init tags |> Maybe.unwrap [] identity
                    in
                    textBeforeTags ++ init ++ [ newTag ]
                        |> List.map String.trim
                        |> String.join " "
                        |> (\x -> x ++ " ")

                Nothing ->
                    state.text
    in
    setText global newText state


{-| Perform tab completion for the main input field.
-}
tabfill : Global -> DefaultState -> ( DefaultState, Event )
tabfill global state =
    case parseInput state.text of
        Ok (Tasks.Input.Text text tags) ->
            if String.startsWith text projectPrefix then
                setText global projectPrefix state

            else if String.startsWith text searchPrefix then
                setText
                    global
                    (Maybe.unwrap
                        searchPrefix
                        (\p -> searchPrefix ++ p ++ " ")
                        global.project
                    )
                    state

            else
                tabfillTag global state tags [ text ]

        Ok (Tasks.Input.Project text) ->
            let
                prefix : String
                prefix =
                    findMatchingPrefix text global.projects
                        |> findCommonPrefix
                        |> Maybe.withDefault text
            in
            setText global (projectPrefix ++ prefix) state

        Ok (Tasks.Input.Search rule) ->
            tabfillTag
                global
                state
                rule.tags
                (searchPrefix :: rule.snippets)

        Err _ ->
            ( state, None )


setText : Global -> String -> DefaultState -> ( DefaultState, Event )
setText global s state =
    let
        tagsMatchingLast : List String -> Maybe (List String)
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



-- VIEW


view : Style -> Model -> Element Msg
view style model =
    case model of
        Default state ->
            viewTaskInput style state


viewTaskInput : Style -> DefaultState -> Element Msg
viewTaskInput style model =
    let
        tagline : List String -> Element Msg
        tagline tags =
            wrappedRow [ width fill, padding (paddingScale 2), spacing (paddingScale 2) ]
                (List.map Element.text tags)

        suggestions : Element Msg
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
            , Html.Attributes.id "input" |> Element.htmlAttribute
            , Element.below suggestions
            ]
            { onChange = SetText
            , text = model.text
            , placeholder = Just (Element.Input.placeholder [] (Element.text "Add task"))
            , label = Element.Input.labelHidden "Add task"
            }


onKeys : List ( String, msg ) -> Attribute msg
onKeys pairs =
    let
        decodeKey : ( String, msg ) -> D.Decoder ( msg, Bool )
        decodeKey ( key, m ) =
            D.when (D.field "key" D.string) ((==) key) (D.succeed ( m, True ))
    in
    List.map decodeKey pairs
        |> D.oneOf
        |> Html.Events.preventDefaultOn "keydown"
        |> Element.htmlAttribute
