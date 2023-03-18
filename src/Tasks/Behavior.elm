module Tasks.Behavior exposing (update)

import Browser.Dom
import Cmd.Extra as Cmd
import Dict
import List.Extra as List
import Maybe.Extra as Maybe
import Prng.Uuid
import Random.Pcg.Extended as Pcg
import Set exposing (Set)
import Task
import Tasks.Input exposing (parseInput, projectPrefix, searchPrefix)
import Tasks.Interop as Interop
import Tasks.Model as Model exposing (Model, Msg(..), Tag, Task, TaskId, ViewState(..))
import Tasks.Style exposing (darkStyle, lightStyle)
import Tasks.Utils exposing (choose, listOfOne)
import Time


{-| Inserts a new task to the model.
This function steps the model's PRNG to produce an id for the task.
-}
addTaskToModel : (TaskId -> Task) -> Model -> Model
addTaskToModel createTask model =
    let
        ( uuid, seed ) =
            Pcg.step Prng.Uuid.generator model.seed

        id =
            Prng.Uuid.toString uuid
    in
    { model
        | text = ""
        , seed = seed
        , tasks = Dict.insert id (createTask id) model.tasks
    }


{-| If the update fails, log the result and return old model.
-}
tryLog : (a -> Result String a) -> a -> ( a, Cmd msg )
tryLog f val =
    case f val of
        Ok x ->
            ( x, Cmd.none )

        Err e ->
            ( val, Interop.log e )


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


{-| Switch current projdct to the asked one. If it doesn't exist, add it to the list.
-}
switchProject : String -> Model -> Model
switchProject project model =
    if project == "" then
        { model | project = Nothing }

    else if List.any ((==) project) model.projects then
        { model | project = Just project }

    else
        { model
            | projects = project :: model.projects
            , project = Just project
        }


{-| handle what happens when the user submits the main input field by pressing Enter.
This currently applies to all states except Edit.
-}
handleMainInput : Model -> ( Model, Cmd Msg )
handleMainInput model =
    case parseInput model.text of
        Ok (Tasks.Input.Text text tags) ->
            -- TODO: What's this assignment for?
            { model | search = Nothing }
                |> Cmd.withCmd (withTime (AddTask text tags model.project))

        Ok (Tasks.Input.Project project) ->
            model
                |> switchProject project
                |> setText ""
                |> Cmd.withNoCmd

        Ok (Tasks.Input.Search rules) ->
            { model | search = Just rules }
                |> Cmd.withNoCmd

        Err _ ->
            ( model, Interop.log "Parsing failed" )


{-| Complete the last tag in the current query.
Depenss on Model.tagSuggestions to be calculated properly.
`tags` should be the list of tags currently in the query.
Note: currently we reorder tags to appear after other content.
-}
tabfillTag : Model -> List String -> String -> Model
tabfillTag model tags textBeforeTags =
    let
        tagMatching =
            model.tagSuggestions
                |> Maybe.andThen listOfOne

        init =
            List.init tags |> Maybe.unwrap [] identity

        newText =
            case tagMatching of
                Just newTag ->
                    String.join " " (textBeforeTags :: init ++ [ newTag ])
                        ++ " "

                Nothing ->
                    model.text
    in
    setText newText model


{-| Perform tab completion for the main input field.
-}
tabfill : Model -> Result String Model
tabfill model =
    case parseInput model.text of
        Ok (Tasks.Input.Text text tags) ->
            if String.startsWith text projectPrefix then
                Ok (setText projectPrefix model)

            else if String.startsWith text searchPrefix then
                Ok (setText searchPrefix model)

            else
                Ok (tabfillTag model tags text)

        Ok (Tasks.Input.Project text) ->
            let
                prefix =
                    Model.findProjectsMatchingSearch text model.projects
                        |> Model.findCommonPrefix
                        |> Maybe.withDefault text
            in
            Ok (setText (projectPrefix ++ prefix) model)

        Ok (Tasks.Input.Search rule) ->
            Ok
                (tabfillTag
                    model
                    rule.tags
                    (searchPrefix ++ String.join " " rule.snippets)
                )

        Err _ ->
            Err "Parsing failed"


{-| Set new text for the main input field.
This also calculates tab completion suggestions and applies search filters accordingly.
TODO: Eventually we have to either debounce search or only set it on submit.
-}
setText : String -> Model -> Model
setText s model =
    let
        tagsMatchingLast tags =
            List.last tags
                |> Maybe.map (Model.findMatchingTags model.tags)
    in
    case parseInput s of
        Ok (Tasks.Input.Search rule) ->
            { model
                | text = s
                , search = Just rule
                , tagSuggestions = tagsMatchingLast rule.tags
            }

        Ok (Tasks.Input.Text _ tags) ->
            { model
                | text = s
                , search = Nothing
                , tagSuggestions = tagsMatchingLast tags
            }

        _ ->
            { model
                | text = s
                , search = Nothing
                , tagSuggestions = Nothing
            }


{-| Removes the given task.
-}
removeTask : TaskId -> Model -> Model
removeTask id model =
    { model | tasks = Dict.remove id model.tasks }


{-| Toggle between dark and light style.
-}
toggleStyle : Model -> Model
toggleStyle model =
    { model | style = choose darkStyle lightStyle (model.style == lightStyle) }


{-| Switch to a project. If already in that project, clear selection.
If `clearText`, clears the main input field.
-}
setProject : Bool -> String -> Model -> Model
setProject clearText target model =
    { model | project = nothingIfUnchanged target model.project }
        |> setText (choose "" model.text clearText)


{-| Delete a project.
This doesn't actually touch tasks, only removes it from the list.
-}
deleteProject : String -> Model -> Model
deleteProject target model =
    { model
        | projects = List.filter ((/=) target) model.projects
        , project = choose Nothing model.project (model.project == Just target)
    }


{-| Copy over tasks and projects from a loaded model.
-}
loadModel : Model.StoredModel -> Model -> Model
loadModel m model =
    { model
        | tasks = m.tasks
        , projects = m.projects
    }


{-| Sets the view state and updates global statd accordingly.
-}
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


{-| Select a task. Keeps note of view state hierarchy.
-}
selectTask : TaskId -> Model -> Model
selectTask id model =
    let
        oldState =
            case model.viewState of
                -- We don't want to nest selections.
                Selected _ state ->
                    state

                state ->
                    state
    in
    setViewState (Selected id oldState) model


{-| Mark task as done at current time.
If it was already done, marks it undone.
-}
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
        SetText s ->
            Cmd.withNoCmd << setText s

        SubmitInput ->
            handleMainInput

        Tabfill ->
            tryLog tabfill

        RemoveTask id ->
            Cmd.withNoCmd << removeTask id

        ToggleStyle ->
            Cmd.withNoCmd << toggleStyle

        SetProject clearText target ->
            Cmd.withNoCmd << setProject clearText target

        DeleteProject target ->
            Cmd.withNoCmd << deleteProject target

        LoadModel m ->
            Cmd.withNoCmd << loadModel m

        SetViewState state ->
            Cmd.withNoCmd << setViewState state

        SelectTask id ->
            Cmd.withNoCmd << selectTask id

        FocusInput ->
            onlyCmd <| always focusInput

        AddTask text tags project time ->
            Cmd.withNoCmd << addTaskToModel (Task text tags project time Nothing)

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


{-| Finish editing a task and switch back to the default view state.
TODO: Switch the state one step "up" instead, so we could return to ShowDone etc.
-}
finishEditState : Task -> Model -> Model
finishEditState task model =
    Model.updateTask (\t -> { t | text = model.text })
        task.id
        { model
            | viewState = Model.None
            , text = ""
        }


{-| Middleware to deal with the Edit state.
Captures SubmitInput and Tabfill messages and lets the rest pass through.
-}
handleEditState : Update -> Update
handleEditState updater msg model =
    case ( model.viewState, msg ) of
        ( Edit task, SubmitInput ) ->
            finishEditState task model
                |> Cmd.withNoCmd

        ( Edit _, Tabfill ) ->
            -- We don't want to do anything here
            model |> Cmd.withNoCmd

        _ ->
            updater msg model


{-| Updates the filteredTasks list and the tag set _exhaustively_.
This will check every single task. Slow, but works for now. :clueless:
-}
updateFilteredTasks : Model -> Model
updateFilteredTasks model =
    let
        sortRule v =
            case v of
                Selected _ x ->
                    sortRule x

                ShowDone ->
                    negate << Maybe.unwrap 0 Time.posixToMillis << .doneAt

                _ ->
                    negate << Time.posixToMillis << .createdAt

        -- TODO: This is a temporary hack.
        -- It is highly unwise to go over every single task for tags again and again.
        tags =
            Dict.toList model.tasks
                |> List.map Tuple.second
                |> List.concatMap .tags
                |> Set.fromList

        filterTasks =
            Model.filterTasks
                { project = model.project
                , done = Model.showDoneTasks model.viewState
                , search = model.search
                }
    in
    { model
        | filteredTasks =
            Dict.toList model.tasks
                |> List.map Tuple.second
                |> filterTasks
                |> List.sortBy (sortRule model.viewState)
        , tags = tags
    }


{-| Middleware to apply filtdrs after update.
-}
filterTasksAfterUpdate : Update -> Update
filterTasksAfterUpdate =
    executeIfChanged
        (\m -> ( m.tasks, m.project, ( Model.showDoneTasks m.viewState, m.search ) ))
        (Cmd.withNoCmd << updateFilteredTasks)


{-| Middleware to save tasks after update.
-}
saveChangedTasks : Update -> Update
saveChangedTasks =
    executeIfChanged (\m -> ( m.tasks, m.projects )) (onlyCmd Interop.save)
        |> disableIf Model.isLoadModel


{-| The central update function. All messages arrive here.
-}
update : Update
update =
    handleMsg
        |> handleEditState
        |> filterTasksAfterUpdate
        |> saveChangedTasks
