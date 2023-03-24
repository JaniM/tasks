module Tasks.Store exposing
    ( Store
    , addTask
    , emptyStore
    , loadTasks
    , removeTask
    , updateFilter
    , updateSort
    , updateTask
    )

import Dict exposing (Dict)
import List.Extra
import Maybe.Extra as Maybe
import Set exposing (Set)
import Tasks.Task exposing (SearchRule, Task, TaskId, emptySwarch)
import Time


type alias Aggregate a k =
    { data : List a
    , contained : Set k
    , key : a -> k
    , filter : a -> Bool
    , sort : List a -> List a
    }


emptyAggregate : (a -> k) -> Aggregate a k
emptyAggregate key =
    { data = []
    , contained = Set.empty
    , key = key
    , filter = always True
    , sort = identity
    }


withSort : (List a -> List a) -> Aggregate a k -> Aggregate a k
withSort sort aggregate =
    { aggregate
        | sort = sort
        , data = sort aggregate.data
    }


withFilter : (a -> Bool) -> List a -> Aggregate a comparable -> Aggregate a comparable
withFilter filter data aggregate =
    { aggregate | filter = filter }
        |> reset data


reset : List a -> Aggregate a comparable -> Aggregate a comparable
reset data aggregate =
    let
        ndata : List a
        ndata =
            data
                |> List.filter aggregate.filter
                -- This is a bit slow, but will do for now
                |> List.Extra.uniqueBy aggregate.key
                |> aggregate.sort

        contained : Set comparable
        contained =
            ndata
                |> List.map aggregate.key
                |> Set.fromList
    in
    { aggregate
        | data = ndata
        , contained = contained
    }


add : a -> Aggregate a comparable -> Aggregate a comparable
add item aggregate =
    if aggregate.filter item && not (Set.member (aggregate.key item) aggregate.contained) then
        { aggregate
            | data = aggregate.sort (item :: aggregate.data)
            , contained = Set.insert (aggregate.key item) aggregate.contained
        }

    else
        aggregate


addMany : List a -> Aggregate a comparable -> Aggregate a comparable
addMany items aggregate =
    List.foldl add aggregate items


remove : comparable -> Aggregate a comparable -> Aggregate a comparable
remove key aggregate =
    if Set.member key aggregate.contained then
        { aggregate
            | data = List.filter (\x -> aggregate.key x /= key) aggregate.data
            , contained = Set.remove key aggregate.contained
        }

    else
        aggregate


update : a -> Aggregate a comparable -> Aggregate a comparable
update item aggregate =
    let
        key : comparable
        key =
            aggregate.key item

        selector : a -> Bool
        selector x =
            aggregate.key x == key
    in
    case ( Set.member key aggregate.contained, aggregate.filter item ) of
        ( True, True ) ->
            -- Used to exist and should still exist -> replace
            { aggregate
                | data = replaceinList selector item aggregate.data
            }

        ( True, False ) ->
            -- Used to exist, but shouldn't anymore -> remove
            { aggregate
                | data = List.Extra.filterNot selector aggregate.data
                , contained = Set.remove key aggregate.contained
            }

        ( False, True ) ->
            -- Didn't use to exist, but should now -> add
            add item aggregate

        ( False, False ) ->
            -- Didn't use to exist and shouldn't exist -> do nothing
            aggregate


replaceinList : (a -> Bool) -> a -> List a -> List a
replaceinList pred item list =
    let
        folder : a -> List a -> List a
        folder x acc =
            if pred x then
                item :: acc

            else
                x :: list
    in
    List.foldr folder [] list


type alias Store =
    { tasks : Dict TaskId Task
    , filteredTasks : Aggregate Task TaskId
    , tags : Aggregate String String
    }


type alias Filter =
    { done : Bool
    , search : SearchRule
    }


filterAggregate : Aggregate Task TaskId
filterAggregate =
    emptyAggregate .id
        |> withSort (List.sortBy (negate << Time.posixToMillis << .createdAt))
        |> withFilter (filterTask { done = False, search = emptySwarch }) []


tagAggregate : Aggregate String String
tagAggregate =
    emptyAggregate identity
        |> withSort List.sort


updateSort : (Task -> Int) -> Store -> Store
updateSort sorter model =
    { model | filteredTasks = withSort (List.sortBy sorter) model.filteredTasks }


emptyStore : Store
emptyStore =
    { tasks = Dict.empty
    , filteredTasks = filterAggregate
    , tags = tagAggregate
    }


loadTasks : Dict TaskId Task -> Store -> Store
loadTasks tasks model =
    let
        all : List Task
        all =
            Dict.toList tasks |> List.map Tuple.second

        tags : List String
        tags =
            Dict.toList tasks
                |> List.map Tuple.second
                |> List.concatMap .tags
    in
    { model
        | tasks = tasks
        , filteredTasks = reset all model.filteredTasks
        , tags = reset tags model.tags
    }


addTask : Task -> Store -> Store
addTask task model =
    { model
        | tasks = Dict.insert task.id task model.tasks
        , filteredTasks = add task model.filteredTasks
        , tags = addMany task.tags model.tags
    }


removeTask : TaskId -> Store -> Store
removeTask taskId model =
    { model
        | tasks = Dict.remove taskId model.tasks
        , filteredTasks = remove taskId model.filteredTasks
    }


updateTask : TaskId -> (Task -> Task) -> Store -> Store
updateTask id updater model =
    case Dict.get id model.tasks of
        Nothing ->
            model

        Just task ->
            let
                newTask =
                    updater task

                newTasks =
                    Dict.insert id newTask model.tasks
            in
            { model
                | tasks = newTasks
                , filteredTasks = update newTask model.filteredTasks
                , tags = addMany newTask.tags model.tags
            }


updateFilter : Filter -> Store -> Store
updateFilter rule model =
    { model
        | filteredTasks =
            withFilter
                (filterTask rule)
                (allTasks model)
                model.filteredTasks
    }


allTasks : Store -> List Task
allTasks model =
    model.tasks
        |> Dict.toList
        |> List.map Tuple.second


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
