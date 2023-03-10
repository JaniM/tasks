module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy
import Html exposing (Html)
import Html.Events as HtmlEvents
import Json.Decode as D
import Json.Decode.Extra as D
import List.Extra as List
import Maybe.Extra as Maybe
import Prng.Uuid
import Random.Pcg.Extended as Pcg
import String exposing (right)
import Tasks.Input exposing (..)
import Tasks.Interop as Interop
import Tasks.Model exposing (..)
import Tasks.Style exposing (..)
import Tasks.Utils exposing (..)
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
subscriptions model =
    Interop.subscribe <|
        \fromJs ->
            case fromJs of
                Interop.Error _ ->
                    LoadModel model

                Interop.LoadModel m ->
                    LoadModel m


type alias Filter =
    { project : Maybe String
    }


projectSearch : Model -> Maybe String
projectSearch model =
    case parseInput model.text of
        Ok (Tasks.Input.Project text) ->
            Just text

        _ ->
            Nothing


addTaskToModel : String -> Maybe String -> Model -> Model
addTaskToModel text project model =
    let
        ( uuid, seed ) =
            Pcg.step Prng.Uuid.generator model.seed
    in
    { model
        | text = ""
        , seed = seed
        , tasks = Task text project uuid :: model.tasks
    }


handleMainInput : Model -> Result String Model
handleMainInput model =
    case parseInput model.text of
        Ok (Tasks.Input.Text text) ->
            Ok (addTaskToModel text model.project model)

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
            Ok newModel

        Err _ ->
            Err "Parsing failed"


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


setText s model =
    { model | text = s }


removeTask id model =
    { model | tasks = List.filter (\t -> t.id /= id) model.tasks }


toggleStyle model =
    { model | style = choose darkStyle lightStyle (model.style == lightStyle) }


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


deleteProject target model =
    { model
        | projects = List.filter ((/=) target) model.projects
        , project = choose Nothing model.project (model.project == Just target)
    }


loadModel m model =
    { model
        | tasks = m.tasks
        , projects = m.projects
    }


type alias Update =
    Msg -> Model -> ( Model, Cmd Msg )


handleMsg : Update
handleMsg msg =
    case msg of
        SetText s ->
            noCmd <| setText s

        SubmitInput ->
            tryLog handleMainInput

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


disableIf pred mw f msg =
    if pred msg then
        f msg

    else
        mw f msg


update : Update
update =
    handleMsg |> disableIf isLoadModel saveChangedTasks


paddingScale : Int -> Int
paddingScale n =
    5 * n


view : Model -> Html Msg
view model =
    layout
        [ Background.color model.style.background
        , Font.color model.style.textColor
        , Font.size (model.style.textSize 1)
        ]
        (topView model)


filterTasks : Filter -> List Task -> List Task
filterTasks { project } =
    List.filter (\x -> Maybe.unwrap True (\_ -> x.project == project) project)


leftBarWidth : Length
leftBarWidth =
    px 150


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
        right =
            case projectSearch model of
                Just text ->
                    viewProjectSearch model.style text model.projects

                Nothing ->
                    Element.Lazy.lazy3 viewTasks model.style model.project model.tasks
    in
    row [ width fill, height fill, clip ]
        [ projectList model.style model.project model.projects
        , right
        ]


projectList : Style -> Maybe String -> List String -> Element Msg
projectList style chosenProject projects =
    column
        [ width leftBarWidth
        , height fill
        , padding (paddingScale 2)
        , spacing (paddingScale 1)
        , Font.size (style.textSize -1)
        ]
        (List.map (projectCard style chosenProject) projects)


projectCard : Style -> Maybe String -> String -> Element Msg
projectCard style chosenProject project =
    el
        [ width fill
        , Background.color
            (choose style.buttonBackground
                style.taskBackground
                (chosenProject == Just project)
            )
        , padding (paddingScale 1)
        , onClick (SetProject False project)
        ]
        (paragraph [] [ text project ])


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
            ]
            { onChange = SetText
            , text = model.text
            , placeholder = Just (Input.placeholder [] (text "Add task"))
            , label = Input.labelHidden "Add task"
            }


viewProjectSearch : Style -> String -> List String -> Element Msg
viewProjectSearch style prefix projects =
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


viewTasks : Style -> Maybe String -> List Task -> Element Msg
viewTasks style project tasks =
    let
        filter =
            { project = project }

        task_ task =
            ( Prng.Uuid.toString task.id
            , viewTask style task
            )
    in
    case ( filterTasks filter tasks, project ) of
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


viewTask : Style -> Task -> Element Msg
viewTask style task =
    let
        remove =
            Input.button
                [ padding (paddingScale 1)
                , Background.color style.buttonBackground
                , Font.size (style.textSize -1)
                ]
                { onPress = Just (RemoveTask task.id)
                , label = text "Remove"
                }
    in
    row
        [ width fill
        , padding (paddingScale 1)
        , Background.color style.taskBackground
        , spacing (paddingScale 1)
        ]
        [ el [ width fill ] (text task.text)
        , text (Maybe.withDefault "No project" task.project)
        , remove
        ]


onKeys : List ( String, msg ) -> Attribute msg
onKeys pairs =
    let
        decodeKey (key, m) =
            D.when (D.field "key" D.string) ((==) key) (D.succeed ( m, True ))
    in
    List.map decodeKey pairs
        |> D.oneOf
        |> HtmlEvents.preventDefaultOn "keydown"
        |> htmlAttribute
