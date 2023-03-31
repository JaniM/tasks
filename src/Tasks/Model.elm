module Tasks.Model exposing
    ( Model
    , Msg(..)
    , StoredModel
    , Tag
    , ViewState(..)
    , emptyModel
    , isLoadModel
    , showDoneTasks
    , updateTask
    )

import Dict exposing (Dict)
import Random.Pcg.Extended as Pcg
import Tasks.MainInput
import Tasks.Store as Store exposing (Store)
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
    { store : Store
    , projects : List String
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
    { store = Store.emptyStore
    , projects = []
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


updateTask : (Task -> Task) -> TaskId -> Model -> Model
updateTask f id model =
    { model | store = Store.updateTask id f model.store }


showDoneTasks : ViewState -> Bool
showDoneTasks v =
    case v of
        Selected _ x ->
            showDoneTasks x

        ShowDone ->
            True

        _ ->
            False
