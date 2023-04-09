module Main exposing (main)

import Browser
import Browser.Events
import Element
    exposing
        ( Attribute
        , Element
        , Length
        , alignTop
        , centerX
        , centerY
        , clip
        , column
        , el
        , fill
        , focused
        , height
        , layout
        , padding
        , paragraph
        , px
        , rgba
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as D
import List.Extra as List
import Random.Pcg.Extended as Pcg
import Task
import Tasks.Behavior
import Tasks.Interop as Interop
import Tasks.MainInput
import Tasks.Model as Model exposing (Keyboard(..), Model, Msg(..), ViewState(..), emptyModel)
import Tasks.Store as Store
import Tasks.Style exposing (Style, paddingScale)
import Tasks.Task exposing (Task, searchProject)
import Tasks.Utils exposing (choose, decodeControlKeys, decodeKeys, findMatchingPrefix)
import Tasks.Views.Help
import Tasks.Views.Listing
import Time


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

        toggleShowDone : Msg
        toggleShowDone =
            SetViewState <|
                setKind
                    (choose
                        Model.Undone
                        Model.Done
                        (Model.viewListKind model.viewState == Model.Done)
                    )
                    model.viewState

        vimNavigation : List ( String, Msg )
        vimNavigation =
            [ ( "k", KeyDown Up )
            , ( "j", KeyDown Down )
            , ( "h", KeyDown Left )
            , ( "l", KeyDown Right )
            , ( "e", KeyDown SelectInput )
            , ( "d", toggleShowDone )
            ]

        keys : List ( String, Msg )
        keys =
            [ ( "ArrowUp", KeyDown Up )
            , ( "ArrowDown", KeyDown Down )
            , ( "ArrowLeft", KeyDown Left )
            , ( "ArrowRight", KeyDown Right )
            , ( "Enter", KeyDown Enter )
            , ( " ", KeyDown Enter )
            , ( "Escape", KeyDown Escape )
            ]
                ++ choose [] vimNavigation (model.selection == Model.InputSelected)
    in
    Sub.batch
        [ Interop.subscribe interopHandler
        , Time.every (60 * 1000) (SetTime model.timeZone)
        , D.oneOf
            [ decodeControlKeys vimNavigation
            , decodeKeys keys
            ]
            |> Browser.Events.onKeyDown
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
               , Element.inFront <| Element.map Help <| Tasks.Views.Help.view model.style model.help
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
            case ( Tasks.MainInput.projectSearch model.mainInput, model.store.filteredTasks, state.project ) of
                ( Just text, _, _ ) ->
                    viewProjectSearch model text

                ( _, [], Just p ) ->
                    viewEmptyProject model.style p

                ( _, filteredTasks, _ ) ->
                    Tasks.Views.Listing.selectList styleWithTime state filteredTasks model.selection

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
