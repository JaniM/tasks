port module Tasks.Interop exposing
    ( FromJs(..)
    , load
    , log
    , save
    , subscribe
    )

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Extra as D
import Json.Encode as E
import Result.Extra as Result
import Tasks.Model exposing (Model, StoredModel)
import Tasks.Task exposing (Priority(..), Task, TaskId)
import Time


port log : String -> Cmd msg


port requestJs : E.Value -> Cmd msg


port fromJs : (D.Value -> msg) -> Sub msg


{-| Message to be sent to JS.
-}
type ToJs
    = Save Model
    | Load


{-| Message that has been received from JS.
-}
type FromJs
    = LoadModel StoredModel
    | Error


{-| Request JS to save the model.
-}
save : Model -> Cmd msg
save =
    performRequest << Save


{-| Request JS to load the model.
This doesn't return anything, the data is received as `LoadModel` from `subscribe`.
-}
load : Cmd msg
load =
    performRequest Load


{-| Converts the request to JSON and passes it to the host.
-}
performRequest : ToJs -> Cmd msg
performRequest =
    requestJs << encodeRequest


{-| Subscribe to messages from the Javascript host.
-}
subscribe : (FromJs -> msg) -> Sub msg
subscribe createMsg =
    D.decodeValue responseDecoder
        >> Result.unwrap Error identity
        >> createMsg
        |> fromJs


encodeRequest : ToJs -> E.Value
encodeRequest req =
    case req of
        Save model ->
            E.object
                [ ( "key", E.string "save" )
                , ( "data", encodeModel model )
                ]

        Load ->
            E.object [ ( "key", E.string "load" ) ]


responseDecoder : D.Decoder FromJs
responseDecoder =
    D.field "key" D.string |> D.andThen selectResponse


selectResponse : String -> D.Decoder FromJs
selectResponse key =
    case key of
        "loadModel" ->
            D.map LoadModel <| D.field "data" decodeModel

        _ ->
            D.fail ""


encodeOptionalField : String -> (a -> E.Value) -> Maybe a -> Maybe ( String, E.Value )
encodeOptionalField key encoder value =
    case value of
        Just v ->
            Just ( key, encoder v )

        _ ->
            Nothing


encodeModel : Model -> E.Value
encodeModel model =
    let
        task : Task -> D.Value
        task t =
            [ Just ( "text", E.string t.text )
            , Just ( "tags", E.list E.string t.tags )
            , Just ( "createdAt", encodeTime t.createdAt )
            , encodeOptionalField "doneAt" encodeTime t.doneAt
            , encodeOptionalField "pickedAt" encodeTime t.pickedAt
            , encodePriority t.priority
            ]
                |> List.filterMap identity
                |> E.object
    in
    E.object
        [ ( "tasks", E.dict identity task model.store.tasks )
        , ( "projects", E.list E.string model.projects )
        , ( "showHelp", E.bool model.help.showOnStartup )
        ]


decodeModel : D.Decoder StoredModel
decodeModel =
    let
        task : D.Decoder (TaskId -> Task)
        task =
            D.map6 Task
                (D.field "text" D.string)
                (D.field "tags" (D.list D.string))
                (fieldWithDefault "priority" Medium decodePriority)
                (D.field "createdAt" decodeTime)
                (D.optionalNullableField "doneAt" decodeTime)
                (D.optionalNullableField "pickedAt" decodeTime)

        tasks : D.Decoder (Dict String Task)
        tasks =
            D.dict task
                |> D.field "tasks"
                |> D.map (Dict.map (\id taskf -> taskf id))

        projects : D.Decoder (List String)
        projects =
            D.list D.string
                |> D.field "projects"

        showHelp : D.Decoder Bool
        showHelp =
            D.field "showHelp" D.bool
    in
    D.map3 StoredModel tasks projects showHelp


decodeTime : D.Decoder Time.Posix
decodeTime =
    D.map Time.millisToPosix D.int


encodeTime : Time.Posix -> E.Value
encodeTime =
    E.int << Time.posixToMillis


decodePriority : D.Decoder Priority
decodePriority =
    D.int
        |> D.andThen
            (\str ->
                case str of
                    0 ->
                        D.succeed Low

                    1 ->
                        D.succeed Medium

                    2 ->
                        D.succeed High

                    _ ->
                        D.fail ""
            )


encodePriority : Priority -> Maybe ( String, E.Value )
encodePriority priority =
    case priority of
        Low ->
            Just ( "priority", E.int 0 )

        Medium ->
            Nothing

        High ->
            Just ( "priority", E.int 2 )


fieldWithDefault : String -> a -> D.Decoder a -> D.Decoder a
fieldWithDefault key defaultValue decoder =
    D.optionalField key decoder
        |> D.map (Maybe.withDefault defaultValue)
