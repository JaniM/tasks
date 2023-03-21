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
import Set exposing (Set)
import Tasks.MainInput
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
    { tasks : Dict String Task
    , filteredTasks : List Task
    , projects : List String
    , tags : Set String
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
    { tasks = Dict.empty
    , filteredTasks = []
    , projects = []
    , tags = Set.empty
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


filterTasksBySearch : SearchRule -> List Task -> List Task
filterTasksBySearch { snippets, tags } tasks =
            let
                lowerSnippets : List String
                lowerSnippets =
                    List.map String.toLower snippets
            in
            tasks
                |> List.filter (\task -> List.isEmpty tags || List.any (\tag -> List.member tag tags) task.tags)
                |> List.filter (\task -> List.all (\s -> String.contains s (String.toLower task.text)) lowerSnippets)


type alias Filter =
    { done : Bool
    , search : SearchRule
    }


filterTasks : Filter -> List Task -> List Task
filterTasks filter tasks =
    tasks
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
    { model | tasks = Dict.update id (Maybe.map f) model.tasks }


showDoneTasks : ViewState -> Bool
showDoneTasks v =
    case v of
        Selected _ x ->
            showDoneTasks x

        ShowDone ->
            True

        _ ->
            False
