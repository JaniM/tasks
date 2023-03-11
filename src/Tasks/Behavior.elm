module Tasks.Behavior exposing (update)

import Browser.Dom
import List.Extra as List
import Maybe.Extra as Maybe
import Prng.Uuid exposing (Uuid)
import Random.Pcg.Extended as Pcg
import Task
import Tasks.Input exposing (parseInput, projectPrefix)
import Tasks.Interop as Interop
import Tasks.Model as Model exposing (Model, Msg(..), Task, TaskId, ViewState(..))
import Tasks.Style exposing (darkStyle, lightStyle)
import Tasks.Utils exposing (choose)
import Time


addTaskToModel : String -> Maybe String -> Time.Posix -> Model -> Model
addTaskToModel text project time model =
    let
        ( uuid, seed ) =
            Pcg.step Prng.Uuid.generator model.seed
    in
    { model
        | text = ""
        , seed = seed
        , tasks = Task text project uuid time Nothing :: model.tasks
    }


editTaskInModel : Task -> String -> Model -> Model
editTaskInModel task text model =
    { model
        | tasks = Model.updateTask (\t -> { t | text = text }) model task.id
        , viewState = Model.None
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


removeTask : Uuid -> Model -> Model
removeTask id model =
    { model | tasks = List.filter (\t -> t.id /= id) model.tasks }


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

        Selected _ ->
            { model | viewState = state }

        Edit task ->
            { model | viewState = state, text = task.text }


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
    { model | tasks = Model.updateTask f model id }


focusInput : Cmd Msg
focusInput =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus "input")


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

        FocusInput ->
            onlyCmd <| always focusInput

        AddTask text project time ->
            noCmd <| addTaskToModel text project time

        MarkDone taskId ->
            onlyCmd <| always <| markDone taskId

        UpdateTask taskId f ->
            noCmd <| handleUpdateTask taskId f

        NoOp ->
            noCmd identity


saveChangedTasks : Update -> Update
saveChangedTasks updater msg model =
    let
        comp m =
            ( m.tasks, m.projects )

        ( newModel, cmds ) =
            updater msg model

        saveCmd =
            if comp model /= comp newModel then
                Interop.save newModel

            else
                Cmd.none
    in
    ( newModel, Cmd.batch [ saveCmd, cmds ] )


handleEditState : Update -> Update
handleEditState updater msg model =
    case model.viewState of
        Edit task ->
            case msg of
                SubmitInput ->
                    ( editTaskInModel task model.text model, Cmd.none )

                Tabfill ->
                    -- We don't want to do anything here
                    ( model, Cmd.none )

                _ ->
                    updater msg model

        _ ->
            updater msg model


disableIf : (Msg -> Bool) -> (Update -> Update) -> Update -> Update
disableIf pred mw f msg =
    if pred msg then
        f msg

    else
        mw f msg


update : Update
update =
    handleMsg
        |> handleEditState
        |> disableIf Model.isLoadModel saveChangedTasks
