module Tasks.Model exposing
    ( EditState
    , ListKind(..)
    , ListState
    , Model
    , Msg(..)
    , StoredModel
    , Tag
    , ViewState(..)
    , emptyModel
    , exitEdit
    , isLoadModel
    , project
    , selectedTask
    , showDoneTasks
    , updateTask
    , viewListKind
    )

import Dict exposing (Dict)
import Random.Pcg.Extended as Pcg
import Tasks.MainInput
import Tasks.Store as Store exposing (Store)
import Tasks.Style exposing (Style)
import Tasks.Task exposing (SearchRule, Task, TaskId)
import Time
import Tasks.Utils exposing (epoch)


type alias Tag =
    String


type alias StoredModel =
    { tasks : Dict String Task
    , projects : List String
    }


type alias Model =
    { -- Task database
      store : Store

    -- List of all projects the user has created
    , projects : List String

    -- Random number generator seed for generating IDs
    , seed : Pcg.Seed
    , style : Style
    , viewState : ViewState

    -- Time zone for displaying dates
    , timeZone : Time.Zone
    , currentTime : Time.Posix
    , search : Maybe SearchRule
    , mainInput : Tasks.MainInput.Model
    }


type ViewState
    = ListTasks ListState
    | Edit EditState


type alias ListState =
    { project : Maybe String
    , selected : Maybe TaskId
    , kind : ListKind
    }


type alias EditState =
    { task : Task
    , prev : ViewState
    }


type ListKind
    = Undone
    | Done


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
    | SelectTask (Maybe TaskId)
    | StartEditing Task
    | FocusInput
    | SetTime Time.Zone Time.Posix
    | PickTask TaskId Bool
    | NoOp


emptyModel : Model
emptyModel =
    { store = Store.emptyStore
    , projects = []
    , seed = Pcg.initialSeed 0 []
    , style = Tasks.Style.darkStyle
    , viewState = ListTasks { project = Nothing, selected = Nothing, kind = Undone }
    , currentTime = epoch
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


updateTask : (Task -> Task) -> TaskId -> Model -> Model
updateTask f id model =
    { model | store = Store.updateTask f id model.store }


showDoneTasks : ViewState -> Bool
showDoneTasks v =
    case v of
        ListTasks { kind } ->
            kind == Done

        _ ->
            False


project : Model -> Maybe String
project model =
    viewProject model.viewState


selectedTask : Model -> Maybe TaskId
selectedTask model =
    viewSelected model.viewState


recurseViewState : ViewState -> ListState
recurseViewState view =
    case view of
        Edit { prev } ->
            recurseViewState prev

        ListTasks state ->
            state


viewProject : ViewState -> Maybe String
viewProject =
    recurseViewState >> .project


viewListKind : ViewState -> ListKind
viewListKind =
    recurseViewState >> .kind


viewSelected : ViewState -> Maybe TaskId
viewSelected view =
    case view of
        ListTasks state ->
            state.selected

        Edit _ ->
            Nothing


exitEdit : Model -> Model
exitEdit model =
    let
        viewState : ViewState
        viewState =
            case model.viewState of
                Edit { prev } ->
                    prev

                x ->
                    x
    in
    { model | viewState = viewState }
