module Tasks.Model exposing
    ( Model
    , Msg(..)
    , Task
    , TaskId
    , ViewState(..)
    , countTasks
    , emptyModel
    , filterTasks
    , findCommonPrefix
    , findProjectsMatchingSearch
    , findTask
    , isLoadModel
    , updateTask
    , showDoneTasks
    )

import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Random.Pcg.Extended as Pcg
import Tasks.Input exposing (InputDesc(..))
import Tasks.Style exposing (Style)
import Time


type alias TaskId =
    String


type alias Task =
    { text : String
    , project : Maybe String
    , createdAt : Time.Posix
    , doneAt : Maybe Time.Posix
    , id : TaskId
    }


type alias Model =
    { text : String
    , tasks : Dict String Task
    , filteredTasks : List Task
    , projects : List String
    , project : Maybe String
    , seed : Pcg.Seed
    , style : Style
    , viewState : ViewState
    , timeZone : Time.Zone
    }


type ViewState
    = None
    | Selected TaskId ViewState
    | Edit Task
    | ShowDone


type Msg
    = SetText String
    | SubmitInput
    | Tabfill
    | AddTask String (Maybe String) Time.Posix
    | RemoveTask TaskId
    | UpdateTask TaskId (Task -> Task)
      -- TODO: Maybe this should be generalized to something like SendCmd?
    | MarkDone TaskId
    | ToggleStyle
    | SetProject Bool String
    | DeleteProject String
    | LoadModel Model
    | SetViewState ViewState
    | SelectTask TaskId
    | FocusInput
    | SetTimeZone Time.Zone
    | NoOp


emptyModel : Model
emptyModel =
    { text = ""
    , tasks = Dict.empty
    , filteredTasks = []
    , projects = []
    , seed = Pcg.initialSeed 0 []
    , style = Tasks.Style.darkStyle
    , project = Nothing
    , viewState = None
    , timeZone = Time.utc
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


type alias Filter =
    { project : Maybe String
    , done : Bool
    }


filterTasks : Filter -> List Task -> List Task
filterTasks filter tasks =
    tasks
        |> filterTasksByProject filter.project
        |> List.filter (\t -> Maybe.isJust t.doneAt == filter.done)


countTasks : Dict k Task -> String -> Int
countTasks tasks p =
    tasks
        |> Dict.toList
        |> List.map Tuple.second
        |> filterTasksByProject (Just p)
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

        pred x =
            String.toLower x |> String.startsWith lowerSearch
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