module Tasks.Views.Help exposing (Model, Msg, close, default, open, update, view)

import Element exposing (Element, alignRight, alignTop, column, el, fill, height, none, padding, paddingXY, paragraph, px, row, spacing, text, width)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Tasks.Style exposing (Style, paddingScale)
import Tasks.Utils exposing (choose)


type alias Model =
    { visible : Bool
    , showOnStartup : Bool
    }


type Msg
    = Toggle
    | ShowOnStartup Bool


default : Model
default =
    { visible = True
    , showOnStartup = True
    }


open : Model -> Model
open model =
    { model | visible = True }


close : Model -> Model
close model =
    { model | visible = False }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Toggle ->
            { model | visible = not model.visible }

        ShowOnStartup value ->
            { model | showOnStartup = value }


view : Style -> Model -> Element Msg
view style model =
    if not model.visible then
        Element.none

    else
        el
            [ width fill
            , height fill
            , padding (paddingScale 6)
            ]
            (helpPane style model)


helpPane : Style -> Model -> Element Msg
helpPane style model =
    Element.column
        [ Element.centerX
        , Element.centerY
        , padding (paddingScale 2)
        , Element.Background.color style.buttonBackground
        ]
        [ paragraph
            [ Element.Font.size (style.textSize 2) ]
            [ text "Help" ]
            |> el [ Element.centerX ]
        , sectionDivider
        , row [ alignTop ]
            [ el [ width fill, height fill ] (commands style)
            , creatingTasks style
            ]
        , sectionDivider
        , hotkeys style
        , sectionDivider
        , row
            [ alignRight, spacing (paddingScale 2) ]
            [ showOnStartupButton style model
            , showHideButton style
            ]
        ]


sectionDivider : Element Msg
sectionDivider =
    el
        [ width fill
        , paddingXY 0 (paddingScale 2)
        ]
    <|
        el
            [ width fill
            , Element.Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
            ]
            none


commands : Style -> Element Msg
commands style =
    Element.textColumn
        [ Element.Font.size (style.textSize 1)
        , width fill
        ]
        [ text "Commands"
        , el [ height (px 10) ] none
        , code "/project <name>"
        , indent <| note "create a new project or switch to an existing one"
        , code "/search <text> #tag1 #tag2"
        , indent <| note "search for tasks with the given text and tags"
        , code "/help"
        , indent <| note "show this pane"
        ]


creatingTasks : Style -> Element Msg
creatingTasks style =
    Element.textColumn
        [ Element.Font.size (style.textSize 1)
        , width fill
        ]
        [ text "Creating tasks"
        , el [ height (px 10) ] none
        , code "task1"
        , indent <| note "create a new task"
        , code "task1 #tag1 #tag2"
        , indent <| note "create a new task and assign it tags"
        , indent <| note "projects are just tags"
        , indent <| note "tags can be added to tasks later by editing them"
        , code "task1 \\#tag"
        , indent <| note "use a backslash to escape a tag"
        , code "task1 ++"
        , indent <| note "Prioritize the task"
        , code "task1 --"
        , indent <| note "Deprioritize the task"
        ]


hotkeys : Style -> Element Msg
hotkeys style =
    column [ width fill, Element.Font.size (style.textSize 1) ]
        [ text "Hotkeys"
        , el [ height (px 10) ] none
        , row [ alignTop, width fill ]
            [ Element.textColumn
                [ width fill, height fill, alignTop ]
                [ code "Up/Down"
                , indent <| note "Navigate between tasks"
                , code "Left/Right"
                , indent <| note "Select action on a task"
                , code "E"
                , indent <| note "Select the input field"
                ]
            , Element.textColumn
                [ width fill, height fill, alignTop ]
                [ code "Enter"
                , indent <| note "Activate a button"
                , code "Ctrl + D"
                , indent <| note "Show/hide done tasks"
                ]
            ]
        ]


note : String -> Element Msg
note txt =
    row [] [ el [ height fill ] (text " - "), paragraph [ Element.spacing 0 ] [ text txt ] ]


indent : Element Msg -> Element Msg
indent =
    el [ Element.paddingEach { top = 0, bottom = 0, left = paddingScale 2, right = 0 } ]


code : String -> Element Msg
code txt =
    el [ Element.Font.family [ Element.Font.monospace ] ] (text txt)


showHideButton : Style -> Element Msg
showHideButton style =
    Element.Input.button
        [ Element.Background.color style.taskBackground
        , padding (paddingScale 1)
        ]
        { onPress = Just Toggle
        , label = Element.text "Close"
        }


showOnStartupButton : Style -> Model -> Element Msg
showOnStartupButton style model =
    Element.Input.button
        [ Element.Background.color (choose style.doneBackground style.taskBackground model.showOnStartup)
        , padding (paddingScale 1)
        ]
        { onPress = Just <| ShowOnStartup (not model.showOnStartup)
        , label =
            Element.text
                (if model.showOnStartup then
                    "Hide on startup"

                 else
                    "Show on startup"
                )
        }
