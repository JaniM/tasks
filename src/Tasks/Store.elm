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
import Tasks.Counter as Counter exposing (Counter)
import Tasks.Task exposing (SearchRule, Task, TaskId, emptySwarch, emptyTask)


type alias Store =
    { tasks : Dict TaskId Task
    , filteredTasks : List Task
    , tags : Counter String
    , filter : Filter
    , sort : Task -> Task -> Order
    }


type alias Filter =
    { done : Bool
    , search : SearchRule
    }


updateSort : (Task -> Task -> Order) -> Store -> Store
updateSort sorter model =
    { model
        | filteredTasks = List.sortWith sorter model.filteredTasks
        , sort = sorter
    }


emptyStore : Store
emptyStore =
    { tasks = Dict.empty
    , filteredTasks = []
    , tags = Counter.empty
    , filter = { done = False, search = emptySwarch }
    , sort = \_ _ -> EQ
    }


loadTasks : Dict TaskId Task -> Store -> Store
loadTasks tasks model =
    let
        tags : List String
        tags =
            Dict.values tasks
                |> List.concatMap .tags
    in
    { model
        | tasks = tasks
        , filteredTasks =
            Dict.values tasks
                |> List.filter (filterTask model.filter)
                |> List.sortWith model.sort
        , tags = Counter.addMany tags Counter.empty
    }


addTask : Task -> Store -> Store
addTask task model =
    let
        tasks : Dict TaskId Task
        tasks =
            Dict.insert task.id task model.tasks

        filteredTasks : List Task
        filteredTasks =
            if filterTask model.filter task then
                List.sortWith model.sort (task :: model.filteredTasks)

            else
                model.filteredTasks
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
            Dict.get taskId model.tasks
                |> Maybe.withDefault emptyTask

        tasks : Dict TaskId Task
        tasks =
            Dict.remove taskId model.tasks

        filteredTasks : List Task
        filteredTasks =
            List.filter ((/=) taskId << .id) model.filteredTasks
    in
    { model
        | tasks = tasks
        , filteredTasks = filteredTasks
        , tags = Counter.removeMany task.tags model.tags
    }


updateTask : (Task -> Task) -> TaskId -> Store -> Store
updateTask updater id model =
    case Dict.get id model.tasks of
        Nothing ->
            model

        Just task ->
            let
                newTask : Task
                newTask =
                    updater task

                tasks : Dict TaskId Task
                tasks =
                    Dict.insert id newTask model.tasks

                filteredTasks : List Task
                filteredTasks =
                    if filterTask model.filter newTask then
                        List.sortWith model.sort (newTask :: List.filter ((/=) id << .id) model.filteredTasks)

                    else
                        List.filter ((/=) id << .id) model.filteredTasks
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
    { model
        | filteredTasks =
            Dict.values model.tasks
                |> List.filter (filterTask rule)
                |> List.sortWith model.sort
        , filter = rule
    }


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
    store.tasks
        |> Dict.values
        |> List.filter (filterTask filter)
        |> List.length


nextTask : TaskId -> Store -> Maybe TaskId
nextTask id store =
    store.filteredTasks
        |> List.dropWhile ((/=) id << .id)
        |> List.drop 1
        |> List.head
        |> Maybe.map .id


prevTask : TaskId -> Store -> Maybe TaskId
prevTask id store =
    store.filteredTasks
        |> List.reverse
        |> List.dropWhile ((/=) id << .id)
        |> List.drop 1
        |> List.head
        |> Maybe.map .id


firstTask : Store -> Maybe TaskId
firstTask store =
    store.filteredTasks
        |> List.head
        |> Maybe.map .id


lastTask : Store -> Maybe TaskId
lastTask store =
    store.filteredTasks
        |> List.last
        |> Maybe.map .id


getTask : TaskId -> Store -> Maybe Task
getTask id store =
    Dict.get id store.tasks


hasTask : TaskId -> Store -> Bool
hasTask id store =
    getTask id store
        |> Maybe.isJust
