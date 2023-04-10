module Tasks.Views.Listing exposing (selectList)

import Element
    exposing
        ( Attribute
        , Color
        , Element
        , below
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , padding
        , paddingEach
        , paragraph
        , row
        , scrollbarY
        , spacing
        , text
        , width
        )
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Keyed
import Element.Lazy
import Html.Attributes
import Html.Events
import Json.Decode as D
import Maybe.Extra as Maybe
import Tasks.Model exposing (ListKind(..), ListState, Msg(..), Selection(..))
import Tasks.Style exposing (Style, paddingScale)
import Tasks.Task exposing (Priority(..), Task)
import Tasks.Utils exposing (epoch, groupByKey)
import Time
import Time.Format
import Time.Format.Config.Config_fi_fi


selectList : Style -> ListState -> List Task -> Selection -> Element Msg
selectList styleWithTime state filteredTasks selection =
    case state.kind of
        Undone ->
            Element.Lazy.lazy3
                viewTasks
                styleWithTime
                selection
                filteredTasks

        Done ->
            Element.Lazy.lazy3
                viewDoneTasksTimeline
                styleWithTime
                selection
                filteredTasks


viewTasks : Style -> Selection -> List Task -> Element Msg
viewTasks style selection tasks =
    let
        task_ : Task -> ( String, Element Msg )
        task_ task =
            ( task.id
            , viewTask style (selectedTask selection == Just task.id) task
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
    el [ width fill, height fill, padding (paddingScale 2) ] <|
        Element.column
            [ width fill
            , height fill
            , spacing (paddingScale 2)
            , scrollbarY
            , disableKeys
            ]
            (List.filterMap identity [ lateElem, onTimeElem, unpickedElem ])


disableKeys : Attribute Msg
disableKeys =
    Html.Events.custom "keydown" (D.succeed { message = NoOp, stopPropagation = False, preventDefault = True })
        |> htmlAttribute


taskDropdown : Style -> Task -> Element Msg
taskDropdown style task =
    let
        button : { label : Element msg, onPress : Maybe msg } -> Int -> Element msg
        button config n =
            Element.Input.button
                [ padding (paddingScale 1)
                , Element.Background.color style.buttonBackground
                , Element.Font.size (style.textSize -1)
                , Html.Attributes.id (task.id ++ "-" ++ String.fromInt n)
                    |> htmlAttribute
                ]
                { onPress = config.onPress, label = config.label }

        remove : Int -> Element Msg
        remove =
            button
                { onPress = Just (RemoveTask task.id)
                , label = text "Remove"
                }

        done : Int -> Element Msg
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

        edit : Int -> Element Msg
        edit =
            button
                { onPress = Just (StartEditing task)
                , label = text "Edit"
                }

        pick : () -> Maybe (Int -> Element Msg)
        pick () =
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

        unpick : () -> Maybe (Int -> Element Msg)
        unpick () =
            justIf (Maybe.isJust task.pickedAt)
                (\_ ->
                    button
                        { onPress = Just (PickTask task.id False)
                        , label = text "Unpick"
                        }
                )

        buttons : List (Maybe (Int -> Element Msg))
        buttons =
            if Maybe.isJust task.doneAt then
                [ Just done, Just edit, Just remove ]

            else
                [ pick (), unpick (), Just done, Just edit, Just remove ]
    in
    List.filterMap identity buttons
        |> List.indexedMap (\i f -> f i)
        |> row
            [ Element.Background.color style.taskBackground
            , Element.Border.width 1
            , padding (paddingScale 2)
            , spacing (paddingScale 2)
            ]


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
                case task.priority of
                    Low ->
                        style.lowPrioBackground

                    Medium ->
                        style.taskBackground

                    High ->
                        style.highPrioBackground

        tags : Element Msg
        tags =
            row [ spacing (paddingScale 2) ]
                (List.map (text << String.dropLeft 1) task.tags)
    in
    row
        [ width fill
        , padding (paddingScale 1)
        , Element.Background.color color
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


viewDoneTasksTimeline : Style -> Selection -> List Task -> Element Msg
viewDoneTasksTimeline style selection tasks =
    let
        task_ : Task -> ( String, Element Msg )
        task_ task =
            ( task.id
            , viewTask style (selectedTask selection == Just task.id) task
            )

        posixToDate : Time.Posix -> String
        posixToDate =
            Time.Format.format Time.Format.Config.Config_fi_fi.config "%d.%m.%y" style.timeZone

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
                (equalDate style.timeZone)
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


justIf : Bool -> (() -> a) -> Maybe a
justIf b f =
    if b then
        Just (f ())

    else
        Nothing



-- UTILS


equalDate : Time.Zone -> Time.Posix -> Time.Posix -> Bool
equalDate z a b =
    (Time.toDay z a == Time.toDay z b)
        && (Time.toMonth z a == Time.toMonth z b)
        && (Time.toYear z a == Time.toYear z b)


onClickNoPropagate : msg -> Attribute msg
onClickNoPropagate msg =
    D.succeed ( msg, True )
        |> Html.Events.stopPropagationOn "click"
        |> htmlAttribute


selectedTask : Selection -> Maybe String
selectedTask selection =
    case selection of
        TaskSelected id _ ->
            Just id

        _ ->
            Nothing
