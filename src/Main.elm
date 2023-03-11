module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy
import Html exposing (Html)
import Html.Attributes
import Html.Events as HtmlEvents
import Json.Decode as D
import Json.Decode.Extra as D
import List.Extra as List
import Maybe.Extra as Maybe
import Prng.Uuid exposing (Uuid)
import Random.Pcg.Extended as Pcg
import Task
import Tasks.Input exposing (..)
import Tasks.Interop as Interop
import Tasks.Model exposing (..)
import Tasks.Style exposing (..)
import Tasks.Utils exposing (..)
import Time
import Tuple exposing (first)


main : Program ( Int, List Int ) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Int, List Int ) -> ( Model, Cmd Msg )
init ( seed, seedExtension ) =
    let
        empty =
            Tasks.Model.emptyModel

        start =
            { empty | seed = Pcg.initialSeed seed seedExtension }
    in
    ( start, Interop.load )


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        interop =
            Interop.subscribe <|
                \fromJs ->
                    case fromJs of
                        Interop.Error _ ->
                            NoOp

                        Interop.LoadModel m ->
                            LoadModel m

        keypress =
            Browser.Events.onKeyPress (D.succeed FocusInput)
    in
    Sub.batch [ interop, keypress ]


projectSearch : Model -> Maybe String
projectSearch model =
    case parseInput model.text of
        Ok (Tasks.Input.Project text) ->
            Just text

        _ ->
            Nothing


addTaskToModel : String -> Maybe String -> Time.Posix -> Model -> Model
addTaskToModel text project time model =
    let
        ( uuid, seed ) =
            Pcg.step Prng.Uuid.generator model.seed
    in
    { model
        | text = ""
        , seed = seed
        , tasks = Task text project uuid time :: model.tasks
    }


addTask : String -> Maybe String -> Cmd Msg
addTask text project =
    Task.perform (AddTask text project) Time.now


editTaskInModel : Task -> String -> Model -> Model
editTaskInModel task text model =
    { model
        | tasks = updateTask (\t -> { t | text = text }) model task.id
        , viewState = None
        , text = ""
    }


handleMainInput : Model -> ( Model, Cmd Msg )
handleMainInput model =
    case parseInput model.text of
        Ok (Tasks.Input.Text text) ->
            ( model, addTask text model.project )

        Ok (Tasks.Input.Project project) ->
            let
                newModel =
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
            in
            ( newModel, Cmd.none )

        Err _ ->
            ( model, Interop.log "Parsing failed" )


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
                    findProjectsMatchingSearch text model.projects
                        |> findCommonPrefix
                        |> Maybe.withDefault text
            in
            Ok { model | text = projectPrefix ++ prefix }

        Err _ ->
            Err "Parsing failed"


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
    let
        newProject =
            Maybe.unwrap
                (Just target)
                (fmap (choose Nothing (Just target)) ((==) target))
    in
    { model
        | project = newProject model.project
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
        |> disableIf isLoadModel saveChangedTasks


paddingScale : Int -> Int
paddingScale n =
    5 * n


leftBarWidth : Length
leftBarWidth =
    px 150


view : Model -> Html Msg
view model =
    layout
        [ Background.color model.style.background
        , Font.color model.style.textColor
        , Font.size (model.style.textSize 1)
        , onClick (choose (SetViewState None) NoOp (viewStateIsSelected model))
        ]
        (topView model)


topView : Model -> Element Msg
topView model =
    column [ width fill, height fill ]
        [ topRow model, contentRow model ]


topRow : Model -> Element Msg
topRow model =
    row
        [ width fill ]
        [ toggleStyleButton, viewTaskInput model ]


toggleStyleButton : Element Msg
toggleStyleButton =
    el [ width leftBarWidth, padding (paddingScale 2) ] <|
        Input.button [ noFocusStyle ]
            { onPress = Just ToggleStyle
            , label = text "🌘"
            }


contentRow : Model -> Element Msg
contentRow model =
    let
        listing =
            case projectSearch model of
                Just text ->
                    viewProjectSearch model text

                Nothing ->
                    Element.Lazy.lazy4 viewTasks model.style model.project model.viewState model.tasks

        pane =
            case model.viewState of
                None ->
                    listing

                Selected _ ->
                    listing

                Edit task ->
                    viewTaskEdit model task
    in
    row [ width fill, height fill, clip ]
        [ projectList model
        , pane
        ]


viewTaskEdit : Model -> Task -> Element Msg
viewTaskEdit _ _ =
    el [ width fill, height fill ] (text "Editing task")


projectList : Model -> Element Msg
projectList ({ style, projects } as model) =
    column
        [ width leftBarWidth
        , height fill
        , padding (paddingScale 2)
        , spacing (paddingScale 1)
        , Font.size (style.textSize -1)
        ]
        (List.map (projectCard model) projects)


projectCard : Model -> String -> Element Msg
projectCard model project =
    row
        [ width fill
        , Background.color
            (choose model.style.buttonBackground
                model.style.taskBackground
                (model.project == Just project)
            )
        , padding (paddingScale 1)
        , onClick (SetProject False project)
        ]
        [ paragraph [ width fill ] [ text project ]
        , text (String.fromInt (countTasks model.tasks project))
        ]


noFocusStyle : Attribute msg
noFocusStyle =
    focused
        [ Element.Border.color (rgba 0.0 0.0 0.0 0.0) ]


viewTaskInput : Model -> Element Msg
viewTaskInput model =
    el [ width fill, padding (paddingScale 2) ] <|
        Input.text
            [ onKeys [ ( "Enter", SubmitInput ), ( "Tab", Tabfill ) ]
            , Background.color model.style.taskBackground
            , Input.focusedOnLoad
            , Html.Attributes.id "input" |> htmlAttribute
            ]
            { onChange = SetText
            , text = model.text
            , placeholder = Just (Input.placeholder [] (text "Add task"))
            , label = Input.labelHidden "Add task"
            }


viewProjectSearch : Model -> String -> Element Msg
viewProjectSearch { style, projects } prefix =
    let
        card project =
            el
                [ width (px 150)
                , Background.color <|
                    choose style.buttonBackground style.taskBackground (prefix == project)
                , padding (paddingScale 1)
                , onClick (SetProject True project)
                ]
                (paragraph [] [ text project ])

        suggestions =
            findProjectsMatchingSearch prefix projects
                |> List.map card
                |> column [ spacing (paddingScale 1), height fill ]

        sideText =
            if prefix == "" then
                text "Clear project selection"

            else if List.notMember prefix projects then
                text ("Create project `" ++ prefix ++ "`")

            else
                text ("Open project `" ++ prefix ++ "`")
    in
    row
        [ padding (paddingScale 2)
        , spacing (paddingScale 2)
        , width fill
        , height fill
        ]
        [ suggestions
        , el [ alignTop ] sideText
        ]


viewEmptyProject : Style -> String -> Element Msg
viewEmptyProject style project =
    el [ width fill, centerY ] <|
        column [ spacing (paddingScale 2), centerX ]
            [ el [ centerX ] (text "Nothing here!")
            , Input.button
                [ padding (paddingScale 1)
                , Background.color style.buttonBackground
                , centerX
                ]
                { onPress = Just (DeleteProject project)
                , label = text "Delete Project"
                }
            ]


viewTasks : Style -> Maybe String -> ViewState -> List Task -> Element Msg
viewTasks style project viewState tasks =
    let
        task_ task =
            ( Prng.Uuid.toString task.id
            , viewTask style (viewState == Selected task.id) task
            )
    in
    case ( filterTasksByProject project tasks, project ) of
        ( [], Just p ) ->
            viewEmptyProject style p

        ( filteredTasks, _ ) ->
            Element.Keyed.column
                [ width fill
                , height fill
                , spacing (paddingScale 2)
                , padding (paddingScale 2)
                , scrollbarY
                ]
                (List.map task_ filteredTasks)


viewTask : Style -> Bool -> Task -> Element Msg
viewTask style selected task =
    let
        button =
            Input.button
                [ padding (paddingScale 1)
                , Background.color style.buttonBackground
                , Font.size (style.textSize -1)
                ]

        remove =
            button
                { onPress = Just (RemoveTask task.id)
                , label = text "Remove"
                }

        edit =
            Input.button
                [ padding (paddingScale 1)
                , Background.color style.buttonBackground
                , Font.size (style.textSize -1)
                , onClickNoPropagate (SetViewState (Edit task))
                ]
                { onPress = Nothing
                , label = text "Edit"
                }

        dropdown =
            if selected then
                row
                    [ Background.color style.taskBackground
                    , Element.Border.width 1
                    , padding (paddingScale 2)
                    , spacing (paddingScale 2)
                    ]
                    [ edit, remove ]

            else
                Element.none
    in
    row
        [ width fill
        , padding (paddingScale 1)
        , Background.color
            (choose style.buttonBackground style.taskBackground selected)
        , spacing (paddingScale 1)
        , below dropdown
        , onClickNoPropagate (SetViewState (Selected task.id))
        ]
        [ paragraph [ width fill ] [ text task.text ]
        , text (Maybe.withDefault "No project" task.project)
        ]


onKeys : List ( String, msg ) -> Attribute msg
onKeys pairs =
    let
        decodeKey ( key, m ) =
            D.when (D.field "key" D.string) ((==) key) (D.succeed ( m, True ))
    in
    List.map decodeKey pairs
        |> D.oneOf
        |> HtmlEvents.preventDefaultOn "keydown"
        |> htmlAttribute


onClickNoPropagate : msg -> Attribute msg
onClickNoPropagate msg =
    D.succeed ( msg, True )
        |> HtmlEvents.stopPropagationOn "click"
        |> htmlAttribute
