module Tasks.Store exposing
    ( Filter
    , Store
    , addTask
    , countTasks
    , emptyStore
    , firstTask
    , hasTask
    , lastTask
    , loadTasks
    , nextTask
    , prevTask
    , removeTask
    , updateFilter
    , updateSort
    , updateTask
    )

import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Reactive.Dict exposing (RDict)
import Reactive.List exposing (RList)
import Tasks.Counter as Counter exposing (Counter)
import Tasks.Task exposing (SearchRule, Task, TaskId, emptySwarch, emptyTask)


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


makeFiltered : Filter -> RDict TaskId Task -> (List Task -> List Task) -> RList TaskId Task
makeFiltered filter tasks sort =
    Reactive.List.fromDict tasks
        |> Reactive.List.filter (filterTask filter)
        |> Reactive.List.withPostStep sort


updateSort : (Task -> Task -> Order) -> Store -> Store
updateSort sorter model =
    { model | filteredTasks = Reactive.List.withPostStep (List.sortWith sorter) model.filteredTasks }


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
        , filteredTasks = makeFiltered model.filter rtasks model.filteredTasks.postStep
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


updateTask : (Task -> Task) -> TaskId -> Store -> Store
updateTask updater id model =
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
    { model | filteredTasks = makeFiltered rule model.tasks model.filteredTasks.postStep }


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
        |> List.filter (filterTask filter)
        |> List.length


nextTask : TaskId -> Store -> Maybe TaskId
nextTask id store =
    store.filteredTasks.data
        |> List.dropWhile ((/=) id << .id)
        |> List.drop 1
        |> List.head
        |> Maybe.map .id


prevTask : TaskId -> Store -> Maybe TaskId
prevTask id store =
    store.filteredTasks.data
        |> List.reverse
        |> List.dropWhile ((/=) id << .id)
        |> List.drop 1
        |> List.head
        |> Maybe.map .id


firstTask : Store -> Maybe TaskId
firstTask store =
    store.filteredTasks.data
        |> List.head
        |> Maybe.map .id


lastTask : Store -> Maybe TaskId
lastTask store =
    store.filteredTasks.data
        |> List.last
        |> Maybe.map .id


getTask : TaskId -> Store -> Maybe Task
getTask id store =
    Dict.get id store.tasks.data


hasTask : TaskId -> Store -> Bool
hasTask id store =
    getTask id store
        |> Maybe.isJust
