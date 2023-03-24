module Tasks.Model exposing
    ( Filter
    , Model
    , Msg(..)
    , StoredModel
    , Tag
    , ViewState(..)
    , countTasks
    , emptyModel
    , filterTasks
    , isLoadModel
    , showDoneTasks
    , updateTask
    )

import Dict exposing (Dict)
import Maybe.Extra as Maybe
import Random.Pcg.Extended as Pcg
import Tasks.MainInput
import Tasks.Store as Store exposing (Store)
import Tasks.Style exposing (Style)
import Tasks.Task exposing (SearchRule, Task, TaskId)
import Time


type alias Tag =
    String


type alias StoredModel =
    { tasks : Dict String Task
    , projects : List String
    }


type alias Model =
    { store : Store
    , projects : List String
    , project : Maybe String
    , seed : Pcg.Seed
    , style : Style
    , viewState : ViewState
    , timeZone : Time.Zone
    , search : Maybe SearchRule
    , mainInput : Tasks.MainInput.Model
    }


type ViewState
    = None
    | Selected TaskId ViewState
    | Edit Task
    | ShowDone


type Msg
    = MainInput Tasks.MainInput.Msg
    | AddTask String (List Tag) Time.Posix
    | RemoveTask TaskId
    | UpdateTask TaskId (Task -> Task)
      -- TODO: Maybe this should be generalized to something like SendCmd?
    | MarkDone TaskId
    | ToggleStyle
    | SetProject Bool String
    | DeleteProject String
    | LoadModel StoredModel
    | SetViewState ViewState
    | SelectTask TaskId
    | FocusInput
    | SetTimeZone Time.Zone
    | NoOp


emptyModel : Model
emptyModel =
    { store = Store.emptyStore
    , projects = []
    , seed = Pcg.initialSeed 0 []
    , style = Tasks.Style.darkStyle
    , project = Nothing
    , viewState = None
    , timeZone = Time.utc
    , search = Nothing
    , mainInput = Tasks.MainInput.defaultModel
    }


isLoadModel : Msg -> Bool
isLoadModel msg =
    case msg of
        LoadModel _ ->
            True

        _ ->
            False


filterTasksByTags : List Tag -> List Task -> List Task
filterTasksByTags tags tasks =
    let
        tagIn : Task -> Tag -> Bool
        tagIn task tag =
            List.member tag task.tags

        pred : Task -> Bool
        pred task =
            List.all (tagIn task) tags
    in
    case tags of
        [] ->
            tasks

        _ ->
            List.filter pred tasks


filterTasksBySnippets : List String -> List Task -> List Task
filterTasksBySnippets snippets tasks =
    let
        lowerSnippets : List String
        lowerSnippets =
            List.map String.toLower snippets

        snippetIn : Task -> String -> Bool
        snippetIn task snippet =
            String.contains snippet (String.toLower task.text)

        pred : Task -> Bool
        pred task =
            List.all (snippetIn task) lowerSnippets
    in
    case snippets of
        [] ->
            tasks

        _ ->
            List.filter pred tasks


filterTasksBySearch : SearchRule -> List Task -> List Task
filterTasksBySearch { snippets, tags } tasks =
    tasks
        |> filterTasksByTags tags
        |> filterTasksBySnippets snippets


type alias Filter =
    { done : Bool
    , search : SearchRule
    }


filterTasks : Filter -> List Task -> List Task
filterTasks filter tasks =
    tasks
        -- Filtering out finished tasks first is going to eliminate *most* tasks from the search.
        |> List.filter (\t -> Maybe.isJust t.doneAt == filter.done)
        |> filterTasksBySearch filter.search


countTasks : Dict k Task -> Filter -> Int
countTasks tasks filter =
    tasks
        |> Dict.toList
        |> List.map Tuple.second
        |> filterTasks filter
        |> List.length


updateTask : (Task -> Task) -> TaskId -> Model -> Model
updateTask f id model =
    { model | store = Store.updateTask id f model.store }


showDoneTasks : ViewState -> Bool
showDoneTasks v =
    case v of
        Selected _ x ->
            showDoneTasks x

        ShowDone ->
            True

        _ ->
            False
