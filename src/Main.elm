module Main exposing (main)

import Browser
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
import Prng.Uuid
import Random.Pcg.Extended as Pcg
import Task
import Tasks.Behavior
import Tasks.Input exposing (..)
import Tasks.Interop as Interop
import Tasks.Model exposing (..)
import Tasks.Style exposing (..)
import Tasks.Utils exposing (..)
import Time
import Time.Format
import Time.Format.Config.Config_fi_fi


main : Program ( Int, List Int ) Model Msg
main =
    Browser.element
        { init = init
        , update = Tasks.Behavior.update
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
    ( start
    , Cmd.batch
        [ Interop.load
        , Task.perform SetTimeZone Time.here
        ]
    )


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


paddingScale : Int -> Int
paddingScale n =
    5 * n


leftBarWidth : Length
leftBarWidth =
    px 150


view : Model -> Html Msg
view model =
    let
        newState =
            case model.viewState of
                Selected _ n ->
                    SetViewState n

                _ ->
                    NoOp
    in
    layout
        [ Background.color model.style.background
        , Font.color model.style.textColor
        , Font.size (model.style.textSize 1)
        , onClick newState
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
        [ row [ width leftBarWidth ]
            [ toggleStyleButton
            , showDoneButton model.style model.viewState
            ]
        , viewTaskInput model
        ]


toggleStyleButton : Element Msg
toggleStyleButton =
    el [ padding (paddingScale 2) ] <|
        Input.button [ noFocusStyle ]
            { onPress = Just ToggleStyle
            , label = text "🌘"
            }


showDoneButton : Style -> ViewState -> Element Msg
showDoneButton style current =
    Input.button
        [ noFocusStyle
        , padding (paddingScale 1)
        , Background.color (choose style.buttonBackground style.doneBackground (current /= ShowDone))
        ]
        { onPress = Just <| SetViewState <| choose None ShowDone (current == ShowDone)
        , label = text "Show Done"
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

        pane state =
            case state of
                None ->
                    listing

                Selected _ s ->
                    pane s

                Edit task ->
                    viewTaskEdit model task

                ShowDone ->
                    viewDoneTasksTimeline model.timeZone model.style model.project model.viewState model.tasks
    in
    row [ width fill, height fill, clip ]
        [ projectList model
        , pane model.viewState
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


isSelected : ViewState -> Task -> Bool
isSelected viewState task =
    case viewState of
        Selected id _ ->
            id == task.id

        _ ->
            False


viewTasks : Style -> Maybe String -> ViewState -> List Task -> Element Msg
viewTasks style project viewState tasks =
    let
        task_ task =
            ( Prng.Uuid.toString task.id
            , viewTask style (isSelected viewState task) task
            )
    in
    case ( filterTasks { project = project, done = False } tasks, project ) of
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

        done =
            button
                { onPress = Just (MarkDone task.id)
                , label = text "Done"
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
                    [ done, edit, remove ]

            else
                Element.none

        color =
            if Maybe.isJust task.doneAt then
                style.doneBackground

            else if selected then
                style.buttonBackground

            else
                style.taskBackground
    in
    row
        [ width fill
        , padding (paddingScale 1)
        , Background.color color
        , spacing (paddingScale 1)
        , below dropdown
        , onClickNoPropagate (SelectTask task.id)
        ]
        [ paragraph [ width fill ] [ text task.text ]
        , text (Maybe.withDefault "No project" task.project)
        ]


viewDoneTasksTimeline : Time.Zone -> Style -> Maybe String -> ViewState -> List Task -> Element Msg
viewDoneTasksTimeline zone style project viewState tasks =
    let
        task_ task =
            ( Prng.Uuid.toString task.id
            , viewTask style (isSelected viewState task) task
            )

        posixToDate =
            Time.Format.format Time.Format.Config.Config_fi_fi.config "%d.%m.%y" zone

        group_ ( key, group ) =
            ( key |> Time.posixToMillis |> String.fromInt
            , column [ width fill ]
                [ text (posixToDate key)
                , Element.Keyed.column
                    [ width fill
                    , spacing (paddingScale 2)
                    , Element.paddingEach { left = paddingScale 4, top = paddingScale 2, right = 0, bottom = 0 }
                    ]
                    (List.map task_ group)
                ]
            )

        groups : List ( Time.Posix, List Task )
        groups =
            filterTasks { project = project, done = True } tasks
                |> List.sortBy (\t -> -(Maybe.unwrap 0 Time.posixToMillis t.doneAt))
                |> List.groupWhile
                    (\a b ->
                        Maybe.map2 (equalDate zone) a.doneAt b.doneAt
                            |> Maybe.unwrap False identity
                    )
                |> List.map
                    (\( key, group ) ->
                        ( key.doneAt
                            |> Maybe.unwrap (Time.millisToPosix 0) identity
                        , key :: group
                        )
                    )
    in
    Element.Keyed.column
        [ width fill
        , height fill
        , spacing (paddingScale 2)
        , padding (paddingScale 2)
        , scrollbarY
        ]
        (List.map group_ groups)


equalDate : Time.Zone -> Time.Posix -> Time.Posix -> Bool
equalDate z a b =
    Time.toDay z a == Time.toDay z b && Time.toMonth z a == Time.toMonth z b && Time.toYear z a == Time.toYear z b


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
