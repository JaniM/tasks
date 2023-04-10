module Tasks.Model exposing
    ( EditState
    , Keyboard(..)
    , ListKind(..)
    , ListState
    , Model
    , Msg(..)
    , Selection(..)
    , StoredModel
    , Tag
    , ViewState(..)
    , emptyModel
    , exitEdit
    , isLoadModel
    , previousView
    , project
    , selectedTask
    , showDoneTasks
    , sortRuleByState
    , updateTask
    , viewListKind
    )

import Dict exposing (Dict)
import Maybe.Extra as Maybe
import Random.Pcg.Extended as Pcg
import Tasks.MainInput
import Tasks.Store as Store exposing (Store)
import Tasks.Style exposing (Style)
import Tasks.Task exposing (SearchRule, Task, TaskId)
import Tasks.Utils exposing (comparePosix, epoch)
import Tasks.Views.Help
import Time


type alias Tag =
    String


type alias StoredModel =
    { tasks : Dict String Task
    , projects : List String
    , showHelp : Bool
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
    , selection : Selection
    , help : Tasks.Views.Help.Model
    }


type ViewState
    = ListTasks ListState
    | Edit EditState


type alias ListState =
    { project : Maybe String
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
    | SetTime Time.Zone Time.Posix
    | PickTask TaskId Bool
    | KeyDown Keyboard
    | Help Tasks.Views.Help.Msg
    | NoOp


type Keyboard
    = Up
    | Down
    | Left
    | Right
    | Enter
    | Escape
    | SelectInput


type Selection
    = InputSelected
      -- id of the selected task and the index of the selected button
    | TaskSelected TaskId Int
    | NoSelection


defaultViewState : ViewState
defaultViewState =
    ListTasks { project = Nothing, kind = Undone }


emptyModel : Model
emptyModel =
    { store =
        Store.emptyStore
            |> Store.updateSort (sortRuleByState defaultViewState)
    , projects = []
    , seed = Pcg.initialSeed 0 []
    , style = Tasks.Style.darkStyle
    , viewState = defaultViewState
    , currentTime = epoch
    , timeZone = Time.utc
    , search = Nothing
    , mainInput = Tasks.MainInput.defaultModel
    , selection = InputSelected
    , help = Tasks.Views.Help.default
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
    case model.selection of
        TaskSelected id _ ->
            Just id

        _ ->
            Nothing


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


{-| Sort rules for the list of tasks.
Done tasks: sort by completion date, descending
Undone tasks: sort by picked date, ascending, then by creation date, descending
-}
sortRuleByState : ViewState -> Task -> Task -> Order
sortRuleByState v a b =
    case v of
        Edit { prev } ->
            sortRuleByState prev a b

        ListTasks { kind } ->
            case kind of
                Done ->
                    compare
                        (Maybe.unwrap 0 Time.posixToMillis b.doneAt)
                        (Maybe.unwrap 0 Time.posixToMillis a.doneAt)

                Undone ->
                    case ( a.pickedAt, b.pickedAt ) of
                        ( Just _, Nothing ) ->
                            LT

                        ( Nothing, Just _ ) ->
                            GT

                        ( Just ap, Just bp ) ->
                            comparePosix ap bp

                        _ ->
                            comparePosix b.createdAt a.createdAt


previousView : ViewState -> ViewState
previousView v =
    case v of
        Edit { prev } ->
            prev

        _ ->
            v
