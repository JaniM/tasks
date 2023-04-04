module Tasks.Behavior exposing (Update, update)

import Browser.Dom
import Cmd.Extra as Cmd
import Maybe.Extra as Maybe
import Prng.Uuid
import Random.Pcg.Extended as Pcg
import Task
import Tasks.Counter as Counter
import Tasks.Interop as Interop
import Tasks.MainInput
import Tasks.Model as Model exposing (Model, Msg(..), ViewState(..))
import Tasks.Store as Store
import Tasks.Style exposing (darkStyle, lightStyle)
import Tasks.Task exposing (SearchRule, Task, TaskId, emptySwarch, searchProject)
import Tasks.Utils exposing (choose)
import Time


{-| Inserts a new task to the model.
This function steps the model's PRNG to produce an id for the task.
-}
addTaskToModel : (TaskId -> Task) -> Model -> Model
addTaskToModel createTask model =
    let
        ( uuid, seed ) =
            Pcg.step Prng.Uuid.generator model.seed

        id : String
        id =
            Prng.Uuid.toString uuid
    in
    { model
        | seed = seed
        , store = Store.addTask (createTask id) model.store
    }


onlyCmd : (model -> Cmd msg) -> model -> ( model, Cmd msg )
onlyCmd f x =
    ( x, f x )


nothingIfUnchanged : a -> Maybe a -> Maybe a
nothingIfUnchanged new old =
    if Just new == old then
        Nothing

    else
        Just new



-- MESSAGE HANDLERS


{-| Send a message with current time attached.
-}
withTime : (Time.Posix -> msg) -> Cmd msg
withTime createMsg =
    Task.perform createMsg Time.now


setProjectInView : (Maybe String -> Maybe String) -> ViewState -> ViewState
setProjectInView project view =
    case view of
        ListTasks state ->
            ListTasks { state | project = project state.project }

        Edit state ->
            -- Don't allow changing projects when editing a task.
            Edit state


{-| Switch current projdct to the asked one. If it doesn't exist, add it to the list.
-}
switchProject : String -> Model -> Model
switchProject project model =
    if project == "" then
        { model | viewState = setProjectInView (always Nothing) model.viewState }

    else if List.any ((==) project) model.projects then
        { model | viewState = setProjectInView (always <| Just project) model.viewState }

    else
        { model
            | projects = project :: model.projects
            , viewState = setProjectInView (always <| Just project) model.viewState
        }


handleMainInput : Tasks.MainInput.Msg -> Model -> ( Model, Cmd Msg )
handleMainInput msg model =
    let
        global : Tasks.MainInput.Global
        global =
            { projects = model.projects
            , project = Model.project model
            , tags = Counter.list model.store.tags
            }

        ( input, event ) =
            Tasks.MainInput.update global msg model.mainInput

        newModel : Model
        newModel =
            { model
                | mainInput = input
                , search = Nothing
            }

        addProjectToTags : List String -> List String
        addProjectToTags tags =
            case Model.project model of
                Just p ->
                    tags ++ [ "#" ++ p ]

                Nothing ->
                    tags
    in
    case event of
        Tasks.MainInput.None ->
            ( newModel, Cmd.none )

        Tasks.MainInput.AddTask text tags ->
            newModel
                |> Cmd.withCmd (withTime (AddTask text (addProjectToTags tags)))

        Tasks.MainInput.SetSearch rule ->
            { newModel | search = Just rule }
                |> Cmd.withNoCmd

        Tasks.MainInput.SetProject project ->
            switchProject project newModel
                |> Cmd.withNoCmd

        Tasks.MainInput.Edited taskId text tags ->
            newModel
                |> Model.exitEdit
                |> Model.updateTask (\t -> { t | text = text, tags = tags }) taskId
                |> Cmd.withNoCmd

        Tasks.MainInput.Error err ->
            newModel
                |> Cmd.withCmd (Interop.log err)


{-| Removes the given task.
-}
removeTask : TaskId -> Model -> Model
removeTask id model =
    { model | store = Store.removeTask id model.store }


{-| Toggle between dark and light style.
-}
toggleStyle : Model -> Model
toggleStyle model =
    { model | style = choose darkStyle lightStyle (model.style == lightStyle) }


{-| Switch to a project. If already in that project, clear selection.
If `clearText`, clears the main input field.
-}
setProject : Bool -> String -> Model -> ( Model, Cmd Msg )
setProject clearText target model =
    let
        after : Model -> ( Model, Cmd Msg )
        after =
            if clearText then
                handleMainInput (Tasks.MainInput.SetText "")

            else
                Cmd.withNoCmd
    in
    { model | viewState = setProjectInView (nothingIfUnchanged target) model.viewState }
        |> after


{-| Delete a project.
This doesn't actually touch tasks, only removes it from the list.
-}
deleteProject : String -> Model -> Model
deleteProject target model =
    { model
        | projects = List.filter ((/=) target) model.projects
        , viewState =
            model.viewState
                |> setProjectInView (\p -> choose Nothing p (p == Just target))
    }


{-| Copy over tasks and projects from a loaded model.
-}
loadModel : Model.StoredModel -> Model -> Model
loadModel m model =
    { model
        | store = Store.loadTasks m.tasks model.store
        , projects = m.projects
    }


sortRuleByState : ViewState -> Task -> Int
sortRuleByState v =
    case v of
        Edit { prev } ->
            sortRuleByState prev

        ListTasks { kind } ->
            case kind of
                Model.Undone ->
                    negate << Maybe.unwrap 0 Time.posixToMillis << .doneAt

                Model.Done ->
                    negate << Time.posixToMillis << .createdAt


{-| Sets the view state and updates global statd accordingly.
-}
setViewState : ViewState -> Model -> ( Model, Cmd Msg )
setViewState state model =
    let
        newModel : Model
        newModel =
            { model
                | viewState = state
                , store = Store.updateSort (sortRuleByState state) model.store
            }
    in
    case state of
        Edit { task } ->
            newModel |> handleMainInput (Tasks.MainInput.StartEditing task)

        _ ->
            newModel |> Cmd.withNoCmd


{-| Select a task. Keeps note of view state hierarchy.
-}
selectTask : Maybe TaskId -> Model -> ( Model, Cmd Msg )
selectTask id model =
    let
        state : ViewState
        state =
            case model.viewState of
                ListTasks s ->
                    ListTasks { s | selected = id }

                s ->
                    s
    in
    setViewState state model


startEditing : Task -> Model -> ( Model, Cmd Msg )
startEditing task model =
    let
        state : ViewState
        state =
            Edit { task = task, prev = model.viewState }
    in
    setViewState state model


{-| Mark task as done at current time.
If it was already done, marks it undone.
-}
markDone : TaskId -> Cmd Msg
markDone id =
    let
        updater : Time.Posix -> Task -> Task
        updater time task =
            { task
                | doneAt =
                    choose
                        Nothing
                        (Just time)
                        (Maybe.isJust task.doneAt)
            }
    in
    withTime (UpdateTask id << updater)


{-| Updates a task in the model using a given updater.
-}
handleUpdateTask : TaskId -> (Task -> Task) -> Model -> Model
handleUpdateTask id f model =
    Model.updateTask f id model


{-| Focus the main ibput field.
-}
focusInput : Cmd Msg
focusInput =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus "input")


{-| Sets the time zone. This will only be run once on startup.
-}
setTimeZone : Time.Zone -> Model -> Model
setTimeZone zone model =
    { model | timeZone = zone }


type alias Update =
    Msg -> Model -> ( Model, Cmd Msg )


{-| The base message handler, executed if middlewares don't decide otherwise.
Currently only 'handleEditState\` can block this.
Note: This doesn't take model directly to enforce moving logic to separate functions.
-}
handleMsg : Update
handleMsg msg =
    case msg of
        MainInput inputMsg ->
            handleMainInput inputMsg

        RemoveTask id ->
            Cmd.withNoCmd << removeTask id

        ToggleStyle ->
            Cmd.withNoCmd << toggleStyle

        SetProject clearText target ->
            setProject clearText target

        DeleteProject target ->
            Cmd.withNoCmd << deleteProject target

        LoadModel m ->
            Cmd.withNoCmd << loadModel m

        SetViewState state ->
            setViewState state

        SelectTask id ->
            selectTask id

        StartEditing task ->
            startEditing task

        FocusInput ->
            onlyCmd <| always focusInput

        AddTask text tags time ->
            Cmd.withNoCmd << addTaskToModel (Task text tags time Nothing)

        MarkDone taskId ->
            onlyCmd <| always <| markDone taskId

        UpdateTask taskId f ->
            Cmd.withNoCmd << handleUpdateTask taskId f

        SetTimeZone zone ->
            Cmd.withNoCmd << setTimeZone zone

        NoOp ->
            Cmd.withNoCmd


{-| A middleware massager that executes a function if the result of `comp` changes after update.
-}
executeIfChanged : (Model -> key) -> (Model -> ( Model, Cmd Msg )) -> Update -> Update
executeIfChanged comp mw updater msg model =
    let
        ( newModel, cmd ) =
            updater msg model
    in
    if comp model /= comp newModel then
        mw newModel |> Cmd.addCmd cmd

    else
        ( newModel, cmd )


{-| Disable the middleware if the predicate is true.
-}
disableIf : (Msg -> Bool) -> (Update -> Update) -> Update -> Update
disableIf pred mw f msg =
    if pred msg then
        f msg

    else
        mw f msg


applySearchToStore : Model -> Model
applySearchToStore model =
    let
        search : SearchRule
        search =
            case ( model.search, Model.project model ) of
                ( Just s, _ ) ->
                    s

                ( Nothing, Just p ) ->
                    searchProject p

                ( Nothing, Nothing ) ->
                    emptySwarch
    in
    { model
        | store =
            Store.updateFilter
                { done = Model.showDoneTasks model.viewState
                , search = search
                }
                model.store
                |> Store.updateSort (sortRuleByState model.viewState)
    }


updateFiltersAfterUpdate : Update -> Update
updateFiltersAfterUpdate =
    executeIfChanged
        (\m -> ( Model.project m, m.search, Model.showDoneTasks m.viewState ))
        (Cmd.withNoCmd << applySearchToStore)


{-| Middleware to save tasks after update.
-}
saveChangedTasks : Update -> Update
saveChangedTasks =
    executeIfChanged (\m -> ( m.store.tasks, m.projects )) (onlyCmd Interop.save)
        |> disableIf Model.isLoadModel


{-| The central update function. All messages arrive here.
-}
update : Update
update =
    handleMsg
        |> updateFiltersAfterUpdate
        |> saveChangedTasks
