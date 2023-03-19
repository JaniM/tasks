module Tasks.Model exposing
    ( Model
    , Msg(..)
    , StoredModel
    , Tag
    , ViewState(..)
    , countTasks
    , emptyModel
    , filterTasks
    , findCommonPrefix
    , findMatchingTags
    , findProjectsMatchingSearch
    , findTask
    , isLoadModel
    , showDoneTasks
    , updateTask
    )

import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Random.Pcg.Extended as Pcg
import Set exposing (Set)
import Tasks.MainInput
import Tasks.Style exposing (Style)
import Tasks.Task exposing (..)
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
    | AddTask String (List Tag) (Maybe String) Time.Posix
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


filterTasksByProject : Maybe String -> List Task -> List Task
filterTasksByProject project tasks =
    case project of
        Just p ->
            tasks |> List.filter (\x -> x.project == Just p)

        Nothing ->
            tasks


filterTasksBySearch : Maybe SearchRule -> List Task -> List Task
filterTasksBySearch search tasks =
    case search of
        Just { snippets, tags } ->
            let
                lowerSnippets =
                    List.map String.toLower snippets
            in
            tasks
                |> List.filter (\task -> List.isEmpty tags || List.any (\tag -> List.member tag tags) task.tags)
                |> List.filter (\task -> List.all (\s -> String.contains s (String.toLower task.text)) lowerSnippets)

        Nothing ->
            tasks


type alias Filter =
    { project : Maybe String
    , done : Bool
    , search : Maybe SearchRule
    }


filterTasks : Filter -> List Task -> List Task
filterTasks filter tasks =
    tasks
        |> filterTasksByProject filter.project
        |> List.filter (\t -> Maybe.isJust t.doneAt == filter.done)
        |> filterTasksBySearch filter.search


countTasks : Dict k Task -> Filter -> Int
countTasks tasks filter =
    tasks
        |> Dict.toList
        |> List.map Tuple.second
        |> filterTasks filter
        |> List.length


findTask : Model -> TaskId -> Maybe Task
findTask model id =
    Dict.get id model.tasks


updateTask : (Task -> Task) -> TaskId -> Model -> Model
updateTask f id model =
    { model | tasks = Dict.update id (Maybe.map f) model.tasks }


findCommonPrefix : List String -> Maybe String
findCommonPrefix strings =
    let
        first =
            List.head strings |> Maybe.withDefault ""
    in
    List.reverseRange (String.length first) 1
        |> List.map (\n -> String.slice 0 n first)
        |> List.find (\p -> List.all (String.startsWith p) strings)


findProjectsMatchingSearch : String -> List String -> List String
findProjectsMatchingSearch search projects =
    let
        lowerSearch =
            String.toLower search

        pred =
            String.toLower >> String.startsWith lowerSearch
    in
    List.filter pred projects


showDoneTasks : ViewState -> Bool
showDoneTasks v =
    case v of
        Selected _ x ->
            showDoneTasks x

        ShowDone ->
            True

        _ ->
            False


findMatchingTags : Set String -> String -> List String
findMatchingTags tags tag =
    tags
        |> Set.filter (String.startsWith tag)
        |> Set.toList
        |> List.sort
