module Tasks.Store exposing
    ( Filter
    , Store
    , addTask
    , emptyStore
    , loadTasks
    , removeTask
    , updateFilter
    , updateSort
    , updateTask, countTasks
    )

import Dict exposing (Dict)
import Maybe.Extra as Maybe
import Reactive.Dict exposing (RDict)
import Reactive.List exposing (RList)
import Tasks.Counter as Counter exposing (Counter)
import Tasks.Task exposing (SearchRule, Task, TaskId, emptySwarch, emptyTask)
import Time


type alias Store =
    { tasks : RDict TaskId Task
    , filteredTasks : RList TaskId Task
    , tags : Counter String
    , filter : Filter
    }


type alias Filter =
    { done : Bool
    , search : SearchRule
    }


makeFiltered : Filter -> RDict TaskId Task -> RList TaskId Task
makeFiltered filter tasks =
    Reactive.List.fromDict tasks
        |> Reactive.List.filter (filterTask filter)
        |> Reactive.List.withPostStep (List.sortBy (negate << Time.posixToMillis << .createdAt))


updateSort : (Task -> Int) -> Store -> Store
updateSort sorter model =
    { model | filteredTasks = Reactive.List.withPostStep (List.sortBy sorter) model.filteredTasks }


emptyStore : Store
emptyStore =
    { tasks = Reactive.Dict.empty .id always
    , filteredTasks = Reactive.List.empty .id
    , tags = Counter.empty
    , filter = { done = False, search = emptySwarch }
    }


loadTasks : Dict TaskId Task -> Store -> Store
loadTasks tasks model =
    let
        tags : List String
        tags =
            Dict.values tasks
                |> List.concatMap .tags

        rtasks : RDict TaskId Task
        rtasks =
            Reactive.Dict.fromNormal tasks .id always
    in
    { model
        | tasks = rtasks
        , filteredTasks = makeFiltered model.filter rtasks
        , tags = Counter.addMany tags Counter.empty
    }


addTask : Task -> Store -> Store
addTask task model =
    let
        ( tasks, taskStep ) =
            Reactive.Dict.add task model.tasks

        ( filteredTasks, _ ) =
            Reactive.List.step taskStep model.filteredTasks
    in
    { model
        | tasks = tasks
        , filteredTasks = filteredTasks
        , tags = Counter.addMany task.tags model.tags
    }


removeTask : TaskId -> Store -> Store
removeTask taskId model =
    let
        task : Task
        task =
            Dict.get taskId model.tasks.data
                |> Maybe.unwrap emptyTask identity

        ( tasks, taskStep ) =
            Reactive.Dict.remove task model.tasks

        ( filteredTasks, _ ) =
            Reactive.List.step taskStep model.filteredTasks
    in
    { model
        | tasks = tasks
        , filteredTasks = filteredTasks
        , tags = Counter.removeMany task.tags model.tags
    }


updateTask : TaskId -> (Task -> Task) -> Store -> Store
updateTask id updater model =
    case Dict.get id model.tasks.data of
        Nothing ->
            model

        Just task ->
            let
                newTask : Task
                newTask =
                    updater task

                ( tasks, taskStep ) =
                    Reactive.Dict.update newTask model.tasks

                ( filteredTasks, _ ) =
                    Reactive.List.step taskStep model.filteredTasks
            in
            { model
                | tasks = tasks
                , filteredTasks = filteredTasks
                , tags =
                    model.tags
                        |> Counter.removeMany task.tags
                        |> Counter.addMany newTask.tags
            }


updateFilter : Filter -> Store -> Store
updateFilter rule model =
    { model | filteredTasks = makeFiltered rule model.tasks }


filterTaskByTags : List String -> Task -> Bool
filterTaskByTags tags task =
    let
        tagIn : String -> Bool
        tagIn tag =
            List.member tag task.tags
    in
    List.all tagIn tags


filterTaskBySnippets : List String -> Task -> Bool
filterTaskBySnippets snippets task =
    let
        lowerSnippets : List String
        lowerSnippets =
            List.map String.toLower snippets

        snippetIn : String -> Bool
        snippetIn snippet =
            String.contains snippet (String.toLower task.text)
    in
    List.all snippetIn lowerSnippets


filterTaskBySearch : SearchRule -> Task -> Bool
filterTaskBySearch { snippets, tags } task =
    filterTaskByTags tags task
        && filterTaskBySnippets snippets task


filterTask : Filter -> Task -> Bool
filterTask filter task =
    Maybe.isJust task.doneAt
        == filter.done
        && filterTaskBySearch filter.search task

countTasks : Store -> Filter -> Int
countTasks store filter =
    store.tasks.data
        |> Dict.values
        |> List.filter  (filterTask filter)
        |> List.length
