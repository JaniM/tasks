module Tasks.Model exposing (..)

import List.Extra as List
import Prng.Uuid exposing (Uuid)
import Random.Pcg.Extended as Pcg
import Tasks.Style exposing (Style)
import Time


type alias TaskId =
    Uuid


type alias Task =
    { text : String
    , project : Maybe String
    , id : TaskId
    , createdAt : Time.Posix
    }


type alias Model =
    { text : String
    , tasks : List Task
    , projects : List String
    , project : Maybe String
    , seed : Pcg.Seed
    , style : Style
    , viewState : ViewState
    }


type ViewState
    = None
    | Selected TaskId
    | Edit Task


type Msg
    = SetText String
    | SubmitInput
    | RemoveTask Uuid
    | ToggleStyle
    | SetProject Bool String
    | DeleteProject String
    | Tabfill
    | LoadModel Model
    | SetViewState ViewState
    | FocusInput
    | AddTask String (Maybe String) Time.Posix
    | NoOp


emptyModel : Model
emptyModel =
    { text = ""
    , tasks = []
    , projects = []
    , seed = Pcg.initialSeed 0 []
    , style = Tasks.Style.darkStyle
    , project = Nothing
    , viewState = None
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


countTasks : List Task -> String -> Int
countTasks tasks p =
    tasks |> filterTasksByProject (Just p) |> List.length


viewStateIsSelected : Model -> Bool
viewStateIsSelected model =
    case model.viewState of
        Selected _ ->
            True

        _ ->
            False


findTask : Model -> Uuid -> Maybe Task
findTask model id =
    List.find (\x -> x.id == id) model.tasks
    
updateTask : (Task -> Task) -> Model -> Uuid -> List Task
updateTask f model id =
    List.updateIf (\x -> x.id == id) f model.tasks
