module Tasks.Behavior exposing (update)

import Browser.Dom
import Cmd.Extra as Cmd
import Dict
import List.Extra as List
import Maybe.Extra as Maybe
import Prng.Uuid
import Random.Pcg.Extended as Pcg
import Task
import Tasks.Input exposing (parseInput, projectPrefix)
import Tasks.Interop as Interop
import Tasks.Model as Model exposing (Model, Msg(..), Task, TaskId, ViewState(..), filterTasks)
import Tasks.Style exposing (darkStyle, lightStyle)
import Tasks.Utils exposing (choose)
import Time


addTaskToModel : String -> Maybe String -> Time.Posix -> Model -> Model
addTaskToModel text project time model =
    let
        ( uuid, seed ) =
            Pcg.step Prng.Uuid.generator model.seed

        id =
            Prng.Uuid.toString uuid
    in
    { model
        | text = ""
        , seed = seed
        , tasks = Dict.insert id (Task text project time Nothing id) model.tasks
    }


editTaskInModel : Task -> String -> Model -> Model
editTaskInModel task text model =
    Model.updateTask (\t -> { t | text = text })
        task.id
        { model
            | viewState = Model.None
            , text = ""
        }


switchProject : String -> Model -> Model
switchProject project model =
    if project == "" then
        { model
            | project = Nothing
            , text = ""
        }

    else if List.any ((==) project) model.projects then
        { model
            | project = Just project
            , text = ""
        }

    else
        { model
            | projects = project :: model.projects
            , project = Just project
            , text = ""
        }


tryLog : (a -> Result String a) -> a -> ( a, Cmd msg )
tryLog f val =
    case f val of
        Ok x ->
            ( x, Cmd.none )

        Err e ->
            ( val, Interop.log e )


noCmd : (a -> a) -> a -> ( a, Cmd msg )
noCmd f x =
    ( f x, Cmd.none )


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


addTask : String -> Maybe String -> Cmd Msg
addTask text project =
    Task.perform (AddTask text project) Time.now


handleMainInput : Model -> ( Model, Cmd Msg )
handleMainInput model =
    case parseInput model.text of
        Ok (Tasks.Input.Text text) ->
            ( model, addTask text model.project )

        Ok (Tasks.Input.Project project) ->
            ( switchProject project model, Cmd.none )

        Err _ ->
            ( model, Interop.log "Parsing failed" )


tabfill : Model -> Result String Model
tabfill model =
    case parseInput model.text of
        Ok (Tasks.Input.Text text) ->
            if String.startsWith text projectPrefix then
                Ok { model | text = projectPrefix }

            else
                Ok model

        Ok (Tasks.Input.Project text) ->
            let
                prefix =
                    Model.findProjectsMatchingSearch text model.projects
                        |> Model.findCommonPrefix
                        |> Maybe.withDefault text
            in
            Ok { model | text = projectPrefix ++ prefix }

        Err _ ->
            Err "Parsing failed"


setText : String -> Model -> Model
setText s model =
    { model | text = s }


removeTask : TaskId -> Model -> Model
removeTask id model =
    { model | tasks = Dict.remove id model.tasks }


toggleStyle : Model -> Model
toggleStyle model =
    { model | style = choose darkStyle lightStyle (model.style == lightStyle) }


setProject : Bool -> String -> Model -> Model
setProject clearText target model =
    { model
        | project = nothingIfUnchanged target model.project
        , text = choose "" model.text clearText
    }


deleteProject : String -> Model -> Model
deleteProject target model =
    { model
        | projects = List.filter ((/=) target) model.projects
        , project = choose Nothing model.project (model.project == Just target)
    }


loadModel : Model -> Model -> Model
loadModel m model =
    { model
        | tasks = m.tasks
        , projects = m.projects
    }


setViewState : ViewState -> Model -> Model
setViewState state model =
    case state of
        None ->
            { model | viewState = state }

        Selected _ _ ->
            { model | viewState = state }

        ShowDone ->
            { model | viewState = state }

        Edit task ->
            { model | viewState = state, text = task.text }


selectTask : TaskId -> Model -> Model
selectTask id model =
    let
        oldState =
            case model.viewState of
                Selected _ state ->
                    state

                state ->
                    state
    in
    { model | viewState = Selected id oldState }


markDone : TaskId -> Cmd Msg
markDone id =
    let
        updater time task =
            { task
                | doneAt =
                    choose
                        Nothing
                        (Just time)
                        (Maybe.isJust task.doneAt)
            }
    in
    Task.perform
        (\time -> UpdateTask id (updater time))
        Time.now


handleUpdateTask : TaskId -> (Task -> Task) -> Model -> Model
handleUpdateTask id f model =
    Model.updateTask f id model


focusInput : Cmd Msg
focusInput =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus "input")


setTimeZone : Time.Zone -> Model -> Model
setTimeZone zone model =
    { model | timeZone = zone }


type alias Update =
    Msg -> Model -> ( Model, Cmd Msg )


handleMsg : Update
handleMsg msg =
    case msg of
        SetText s ->
            noCmd <| setText s

        SubmitInput ->
            handleMainInput

        Tabfill ->
            tryLog tabfill

        RemoveTask id ->
            noCmd <| removeTask id

        ToggleStyle ->
            noCmd <| toggleStyle

        SetProject clearText target ->
            noCmd <| setProject clearText target

        DeleteProject target ->
            noCmd <| deleteProject target

        LoadModel m ->
            noCmd <| loadModel m

        SetViewState state ->
            noCmd <| setViewState state

        SelectTask id ->
            noCmd <| selectTask id

        FocusInput ->
            onlyCmd <| always focusInput

        AddTask text project time ->
            noCmd <| addTaskToModel text project time

        MarkDone taskId ->
            onlyCmd <| always <| markDone taskId

        UpdateTask taskId f ->
            noCmd <| handleUpdateTask taskId f

        SetTimeZone zone ->
            noCmd <| setTimeZone zone

        NoOp ->
            noCmd identity


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


disableIf : (Msg -> Bool) -> (Update -> Update) -> Update -> Update
disableIf pred mw f msg =
    if pred msg then
        f msg

    else
        mw f msg


handleEditState : Update -> Update
handleEditState updater msg model =
    case ( model.viewState, msg ) of
        ( Edit task, SubmitInput ) ->
            editTaskInModel task model.text model
                |> Cmd.withNoCmd

        ( Edit _, Tabfill ) ->
            -- We don't want to do anything here
            model |> Cmd.withNoCmd

        _ ->
            updater msg model


updateFilteredTasks : Model -> Model
updateFilteredTasks model =
    let
        sortRule v =
            case v of
                Selected _ x ->
                    sortRule x

                ShowDone ->
                    \t -> -(Maybe.unwrap 0 Time.posixToMillis t.doneAt)

                _ ->
                    \t -> -(Time.posixToMillis t.createdAt)
    in
    { model
        | filteredTasks =
            Dict.toList model.tasks
                |> List.map Tuple.second
                |> filterTasks
                    { project = model.project
                    , done = Model.showDoneTasks model.viewState
                    }
                |> List.sortBy (sortRule model.viewState)
    }


filterTasksAfterUpdate : Update -> Update
filterTasksAfterUpdate =
    executeIfChanged
        (\m -> ( m.tasks, m.project, Model.showDoneTasks m.viewState ))
        (noCmd updateFilteredTasks)


saveChangedTasks : Update -> Update
saveChangedTasks =
    executeIfChanged (\m -> ( m.tasks, m.projects )) (onlyCmd Interop.save)
        |> disableIf Model.isLoadModel


update : Update
update =
    handleMsg
        |> handleEditState
        |> filterTasksAfterUpdate
        |> saveChangedTasks
