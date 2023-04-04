module Main exposing (main)

import Browser
import Browser.Events
import Element
    exposing
        ( Attribute
        , Color
        , Element
        , Length
        , alignTop
        , below
        , centerX
        , centerY
        , clip
        , column
        , el
        , fill
        , focused
        , height
        , htmlAttribute
        , layout
        , padding
        , paddingEach
        , paragraph
        , px
        , rgba
        , row
        , scrollbarY
        , spacing
        , text
        , width
        )
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
import List.Extra as List
import Maybe.Extra as Maybe
import Random.Pcg.Extended as Pcg
import Task
import Tasks.Behavior
import Tasks.Interop as Interop
import Tasks.MainInput
import Tasks.Model as Model exposing (Model, Msg(..), ViewState(..), emptyModel)
import Tasks.Store as Store
import Tasks.Style exposing (Style, paddingScale)
import Tasks.Task exposing (Task, searchProject)
import Tasks.Utils exposing (choose, epoch, findMatchingPrefix, groupByKey)
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
    ( { emptyModel | seed = Pcg.initialSeed seed seedExtension }
    , Cmd.batch
        [ Interop.load
        , Task.map2 SetTime Time.here Time.now
            |> Task.perform identity
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        interopHandler : Interop.FromJs -> Msg
        interopHandler fromJs =
            case fromJs of
                Interop.Error ->
                    NoOp

                Interop.LoadModel m ->
                    LoadModel m
    in
    Sub.batch
        [ Interop.subscribe interopHandler
        , Browser.Events.onKeyDown (D.succeed FocusInput)
        , Time.every (60 * 1000) (SetTime model.timeZone)
        ]


leftBarWidth : Length
leftBarWidth =
    px 150


view : Model -> Html Msg
view model =
    let
        attrs : List (Attribute Msg)
        attrs =
            case Model.selectedTask model of
                Just _ ->
                    [ onClick (SelectTask Nothing) ]

                Nothing ->
                    []
    in
    layout
        (attrs
            ++ [ Background.color model.style.background
               , Font.color model.style.textColor
               , Font.size (model.style.textSize 1)
               ]
        )
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


setKind : Model.ListKind -> Model.ViewState -> Model.ViewState
setKind k v =
    case v of
        Model.Edit state ->
            Model.Edit state

        Model.ListTasks state ->
            Model.ListTasks { state | kind = k }


showDoneButton : Style -> ViewState -> Element Msg
showDoneButton style current =
    let
        kind : Model.ListKind
        kind =
            Model.viewListKind current
    in
    Input.button
        [ noFocusStyle
        , padding (paddingScale 1)
        , Background.color (choose style.buttonBackground style.doneBackground (kind /= Model.Done))
        ]
        { onPress = Just <| SetViewState <| setKind (choose Model.Undone Model.Done (kind == Model.Done)) current
        , label = text "Show Done"
        }


contentRow : Model -> Element Msg
contentRow ({ style } as model) =
    let
        styleWithTime : Style
        styleWithTime =
            { style | currentTime = model.currentTime, timeZone = model.timeZone }

        listing : Model.ListState -> Element Msg
        listing state =
            case ( Tasks.MainInput.projectSearch model.mainInput, model.store.filteredTasks.data, state.project ) of
                ( Just text, _, _ ) ->
                    viewProjectSearch model text

                ( _, [], Just p ) ->
                    viewEmptyProject model.style p

                ( _, filteredTasks, _ ) ->
                    selectList state filteredTasks

        selectList : Model.ListState -> List Task -> Element Msg
        selectList state filteredTasks =
            case state.kind of
                Model.Undone ->
                    Element.Lazy.lazy3 viewTasks styleWithTime state filteredTasks

                Model.Done ->
                    viewDoneTasksTimeline model.timeZone styleWithTime state model.store.filteredTasks.data

        pane : ViewState -> Element Msg
        pane state =
            case state of
                Model.ListTasks s ->
                    listing s

                Edit { task } ->
                    viewTaskEdit model task
    in
    row [ width fill, height fill, clip ]
        [ projectList model
        , pane model.viewState
        ]


viewTaskEdit : Model -> Task -> Element Msg
viewTaskEdit _ _ =
    el [ width fill, height fill, padding (paddingScale 2) ] (text "Editing task")


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
                (Model.project model == Just project)
            )
        , padding (paddingScale 1)
        , onClick (SetProject False project)
        ]
        [ paragraph [ width fill ] [ text project ]
        , Store.countTasks model.store { done = False, search = searchProject project }
            |> String.fromInt
            |> text
        ]


noFocusStyle : Attribute msg
noFocusStyle =
    focused
        [ Element.Border.color (rgba 0.0 0.0 0.0 0.0) ]


viewTaskInput : Model -> Element Msg
viewTaskInput model =
    Tasks.MainInput.view model.style model.mainInput
        |> Element.map MainInput


viewProjectSearch : Model -> String -> Element Msg
viewProjectSearch { style, projects } prefix =
    let
        card : String -> Element Msg
        card project =
            el
                [ width (px 150)
                , Background.color <|
                    choose style.buttonBackground style.taskBackground (prefix == project)
                , padding (paddingScale 1)
                , onClick (SetProject True project)
                ]
                (paragraph [] [ text project ])

        suggestions : Element Msg
        suggestions =
            findMatchingPrefix prefix projects
                |> List.map card
                |> column [ spacing (paddingScale 1), height fill ]

        sideText : Element Msg
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


justIf : Bool -> (() -> a) -> Maybe a
justIf b f =
    if b then
        Just (f ())

    else
        Nothing


viewTasks : Style -> Model.ListState -> List Task -> Element Msg
viewTasks style state tasks =
    let
        task_ : Task -> ( String, Element Msg )
        task_ task =
            ( task.id
            , viewTask style (state.selected == Just task.id) task
            )

        ( picked, unpicked ) =
            List.partition (\t -> Maybe.isJust t.pickedAt) tasks

        ( onTime, late ) =
            List.partition (\t -> equalDate style.timeZone (Maybe.unwrap epoch identity t.pickedAt) style.currentTime) picked

        pickedColumn : List ( String, Element Msg ) -> Element Msg
        pickedColumn =
            Element.Keyed.column
                [ width fill
                , spacing (paddingScale 2)
                , paddingEach { bottom = paddingScale 2, left = 0, right = 0, top = 0 }
                , Element.Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                ]

        lateElem : Maybe (Element Msg)
        lateElem =
            justIf (List.isEmpty late |> not)
                (\_ -> pickedColumn (List.map task_ late))

        onTimeElem : Maybe (Element Msg)
        onTimeElem =
            justIf (List.isEmpty onTime |> not)
                (\_ -> pickedColumn (List.map task_ onTime))

        unpickedElem : Maybe (Element Msg)
        unpickedElem =
            justIf (List.isEmpty unpicked |> not)
                (\_ ->
                    Element.Keyed.column
                        [ width fill
                        , spacing (paddingScale 2)
                        ]
                        (List.map task_ unpicked)
                )
    in
    Element.column
        [ width fill
        , height fill
        , spacing (paddingScale 2)
        , padding (paddingScale 2)
        , scrollbarY
        ]
        (List.filterMap identity [ lateElem, onTimeElem, unpickedElem ])


taskDropdown : Style -> Task -> Element Msg
taskDropdown style task =
    let
        button : { label : Element msg, onPress : Maybe msg } -> Element msg
        button =
            Input.button
                [ padding (paddingScale 1)
                , Background.color style.buttonBackground
                , Font.size (style.textSize -1)
                ]

        remove : Element Msg
        remove =
            button
                { onPress = Just (RemoveTask task.id)
                , label = text "Remove"
                }

        done : Element Msg
        done =
            if Maybe.isJust task.doneAt then
                button
                    { onPress = Just (MarkDone task.id)
                    , label = text "Unmark Done"
                    }

            else
                button
                    { onPress = Just (MarkDone task.id)
                    , label = text "Done"
                    }

        edit : Element Msg
        edit =
            Input.button
                [ padding (paddingScale 1)
                , Background.color style.buttonBackground
                , Font.size (style.textSize -1)
                , onClickNoPropagate (StartEditing task)
                ]
                { onPress = Nothing
                , label = text "Edit"
                }

        pick : Maybe (Element Msg)
        pick =
            if pickedOnDifferentDate style task then
                Just
                    (button
                        { onPress = Just (PickTask task.id True)
                        , label = text "Repick"
                        }
                    )

            else if Maybe.isJust task.pickedAt then
                Nothing

            else
                Just
                    (button
                        { onPress = Just (PickTask task.id True)
                        , label = text "Pick"
                        }
                    )

        unpick : Maybe (Element Msg)
        unpick =
            justIf (Maybe.isJust task.pickedAt)
                (\_ ->
                    button
                        { onPress = Just (PickTask task.id False)
                        , label = text "Unpick"
                        }
                )
        
        buttons : List (Maybe (Element Msg))
        buttons =
            if Maybe.isJust task.doneAt then
                [ Just done, Just edit, Just remove ]

            else
                [ pick, unpick, Just done, Just edit, Just remove ]
    in
    row
        [ Background.color style.taskBackground
        , Element.Border.width 1
        , padding (paddingScale 2)
        , spacing (paddingScale 2)
        ]
        (List.filterMap identity buttons)


viewTask : Style -> Bool -> Task -> Element Msg
viewTask style selected task =
    let
        dropdown : Element Msg
        dropdown =
            if selected then
                taskDropdown style task

            else
                Element.none

        color : Color
        color =
            if Maybe.isJust task.doneAt then
                style.doneBackground

            else if pickedOnDifferentDate style task then
                style.lateBackground

            else if selected then
                style.buttonBackground

            else
                style.taskBackground

        tags : Element Msg
        tags =
            row [ spacing (paddingScale 2) ]
                (List.map (text << String.dropLeft 1) task.tags)
    in
    row
        [ width fill
        , padding (paddingScale 1)
        , Background.color color
        , spacing (paddingScale 2)
        , below dropdown
        , onClickNoPropagate (SelectTask (Just task.id))
        ]
        [ paragraph [ width fill ] [ text task.text ]
        , tags
        ]


pickedOnDifferentDate : Style -> Task -> Bool
pickedOnDifferentDate style task =
    Maybe.map (not << equalDate style.timeZone style.currentTime) task.pickedAt
        |> Maybe.withDefault False


viewDoneTasksTimeline : Time.Zone -> Style -> Model.ListState -> List Task -> Element Msg
viewDoneTasksTimeline zone style state tasks =
    let
        task_ : Task -> ( String, Element Msg )
        task_ task =
            ( task.id
            , viewTask style (state.selected == Just task.id) task
            )

        posixToDate : Time.Posix -> String
        posixToDate =
            Time.Format.format Time.Format.Config.Config_fi_fi.config "%d.%m.%y" zone

        group_ : ( Time.Posix, List Task ) -> ( String, Element Msg )
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
            groupByKey
                (\t -> Maybe.unwrap epoch identity t.doneAt)
                (equalDate zone)
                tasks
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
    (Time.toDay z a == Time.toDay z b)
        && (Time.toMonth z a == Time.toMonth z b)
        && (Time.toYear z a == Time.toYear z b)


onClickNoPropagate : msg -> Attribute msg
onClickNoPropagate msg =
    D.succeed ( msg, True )
        |> HtmlEvents.stopPropagationOn "click"
        |> htmlAttribute
