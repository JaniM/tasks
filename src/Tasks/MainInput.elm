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
import Tasks.Input exposing (parseInput, projectPrefix, searchPrefix)
import Tasks.Style exposing (Style, paddingScale)
import Tasks.Task exposing (SearchRule, Task, TaskId)
import Tasks.Utils exposing (findCommonPrefix, findMatchingPrefix, listOfOne)
import Tuple exposing (mapFirst)


type alias CommonState a =
    { a
        | text : String
        , tagSuggestions : Maybe (List String)
    }


type alias DefaultState =
    CommonState {}


type alias EditState =
    CommonState { taskId : String }


type Model
    = Default DefaultState
    | Edit EditState


type alias Global =
    { tags : List String
    , projects : List String
    , project : Maybe String
    }


type Msg
    = SetText String
    | SubmitInput
    | Tabfill
    | StartEditing Task


type Event
    = None
    | AddTask String (List String)
    | SetSearch SearchRule
    | SetProject String
    | Edited TaskId String (List String)
    | Error String


defaultModel : Model
defaultModel =
    Default defaultState


defaultState : DefaultState
defaultState =
    { text = "", tagSuggestions = Nothing }


findMatchingTags : List String -> String -> List String
findMatchingTags tags tag =
    tags
        |> List.filter (String.startsWith tag)


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


handleMainInputEdit : EditState -> ( Model, Event )
handleMainInputEdit state =
    case parseInput state.text of
        Ok (Tasks.Input.Text text tags) ->
            ( Default defaultState, Edited state.taskId text tags )

        _ ->
            ( Edit state, None )


{-| Complete the last tag in the current query.
Depenss on Model.tagSuggestions to be calculated properly.
`tags` should be the list of tags currently in the query.
Note: currently we reorder tags to appear after other content.
-}
tabfillTag : Global -> CommonState s -> List String -> List String -> ( CommonState s, Event )
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
                    (textBeforeTags ++ init ++ [ newTag ])
                        |> List.map String.trim
                        |> String.join " "
                        |> (\x -> x ++ " ")

                Nothing ->
                    state.text
    in
    setTextDefault global newText state


{-| Perform tab completion for the main input field.
-}
tabfill : Global -> CommonState s -> ( CommonState s, Event )
tabfill global state =
    case parseInput state.text of
        Ok (Tasks.Input.Text text tags) ->
            if String.startsWith text projectPrefix then
                setTextDefault global projectPrefix state

            else if String.startsWith text searchPrefix then
                setTextDefault
                    global
                    (Maybe.unwrap
                        searchPrefix
                        (\p -> searchPrefix ++ "#" ++ p ++ " ")
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
            setTextDefault global (projectPrefix ++ prefix) state

        Ok (Tasks.Input.Search rule) ->
            tabfillTag
                global
                state
                rule.tags
                (searchPrefix :: rule.snippets)

        Err _ ->
            ( state, None )


tagsMatchingLast : Global -> List String -> Maybe (List String)
tagsMatchingLast global tags =
    List.last tags
        |> Maybe.map (findMatchingTags global.tags)


setTextDefault : Global -> String -> CommonState s -> ( CommonState s, Event )
setTextDefault global s state =
    case parseInput s of
        Ok (Tasks.Input.Search rule) ->
            { state
                | text = s
                , tagSuggestions = tagsMatchingLast global rule.tags
            }
                |> withEvent (SetSearch rule)

        Ok (Tasks.Input.Text _ tags) ->
            { state
                | text = s
                , tagSuggestions = tagsMatchingLast global tags
            }
                |> withNoEvent

        _ ->
            { state
                | text = s
                , tagSuggestions = Nothing
            }
                |> withNoEvent


setTextEdit : Global -> String -> EditState -> ( EditState, Event )
setTextEdit global s state =
    case parseInput s of
        Ok (Tasks.Input.Text _ tags) ->
            { state
                | text = s
                , tagSuggestions = tagsMatchingLast global tags
            }
                |> withNoEvent

        _ ->
            state
                |> withNoEvent


editTask : Task -> Model
editTask task =
    Edit
        { text = String.join " " (task.text :: task.tags)
        , tagSuggestions = Nothing
        , taskId = task.id
        }


updateDefault : Global -> Msg -> DefaultState -> ( Model, Event )
updateDefault global msg state =
    case msg of
        SetText text ->
            setTextDefault global text state
                |> mapFirst Default

        SubmitInput ->
            handleMainInput state
                |> mapFirst Default

        Tabfill ->
            tabfill global state
                |> mapFirst Default

        StartEditing task ->
            editTask task
                |> withNoEvent


updateEdit : Global -> Msg -> EditState -> ( Model, Event )
updateEdit global msg state =
    case msg of
        SetText text ->
            setTextEdit global text state
                |> mapFirst Edit

        SubmitInput ->
            handleMainInputEdit state

        Tabfill ->
            tabfill global state
                |> mapFirst Edit

        StartEditing _ ->
            -- This should be unreachable. In case we do reach it, report an error.
            ( Edit state
            , Error "Reached unreachable case: MainInput.updateEdit"
            )


update : Global -> Msg -> Model -> ( Model, Event )
update global msg model =
    case model of
        Default state ->
            updateDefault global msg state

        Edit state ->
            updateEdit global msg state



-- VIEW


view : Style -> Model -> Element Msg
view style model =
    case model of
        Default state ->
            viewTaskInput style state

        Edit state ->
            viewTaskInput style state


viewTaskInput : Style -> CommonState s -> Element Msg
viewTaskInput style model =
    el [ width fill, padding (paddingScale 2) ] <|
        Element.Input.text
            [ onKeys [ ( "Enter", SubmitInput ), ( "Tab", Tabfill ) ]
            , Background.color style.taskBackground
            , Element.Input.focusedOnLoad
            , Html.Attributes.id "input" |> Element.htmlAttribute
            , Element.below (tagSuggestionsBox style model)
            ]
            { onChange = SetText
            , text = model.text
            , placeholder = Just (Element.Input.placeholder [] (Element.text "Add task"))
            , label = Element.Input.labelHidden "Add task"
            }


tagline : Style -> List String -> Element Msg
tagline style tags =
    wrappedRow
        [ width fill
        , padding (paddingScale 2)
        , spacing (paddingScale 2)
        , Element.moveDown 5
        , Background.color style.background
        ]
        (List.map Element.text tags)


tagSuggestionsBox : Style -> CommonState s -> Element Msg
tagSuggestionsBox style state =
    case state.tagSuggestions of
        Just [ tag ] ->
            -- TODO: this is a hack
            if String.contains (" " ++ tag) state.text then
                Element.none

            else
                tagline style [ tag ]

        Just tags ->
            tagline style tags

        Nothing ->
            Element.none


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
