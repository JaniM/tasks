port module Tasks.Interop exposing (FromJs(..), load, log, save, subscribe)

import Dict
import Json.Decode as D
import Json.Decode.Extra as D
import Json.Encode as E
import Json.Encode.Extra as E
import Maybe.Extra
import Result.Extra as Result
import Tasks.Model exposing (Model, Task, TaskId, emptyModel)
import Time


port log : String -> Cmd msg


port requestJs : E.Value -> Cmd msg


port fromJs : (D.Value -> msg) -> Sub msg


type ToJs
    = Save Model
    | Load


type FromJs
    = LoadModel Model
    | Error D.Error


save : Model -> Cmd msg
save =
    performRequest << Save


load : Cmd msg
load =
    performRequest Load


performRequest : ToJs -> Cmd msg
performRequest =
    requestJs << encodeRequest


subscribe : (FromJs -> msg) -> Sub msg
subscribe createMsg =
    D.decodeValue responseDecoder
        >> Result.unpack Error identity
        >> createMsg
        |> fromJs


encodeRequest : ToJs -> E.Value
encodeRequest req =
    case req of
        Save model ->
            E.object [ ( "key", E.string "save" ), ( "data", encodeModel model ) ]

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


encodeModel : Model -> E.Value
encodeModel model =
    let
        task : Task -> D.Value
        task t =
            E.object
                [ ( "text", E.string t.text )
                , ( "tags", E.list E.string t.tags )
                , ( "project", E.maybe E.string t.project )
                , ( "createdAt", encodeTime t.createdAt )
                , ( "doneAt", E.maybe encodeTime t.doneAt )
                ]
    in
    E.object
        [ ( "tasks", E.dict identity task model.tasks )
        , ( "projects", E.list E.string model.projects )
        ]


decodeModel : D.Decoder Model
decodeModel =
    let
        task : D.Decoder (TaskId -> Task)
        task =
            D.map5 Task
                (D.field "text" D.string)
                (D.field "tags" (D.list D.string))
                (D.field "project" (D.maybe D.string))
                (D.field "createdAt" decodeTime)
                (D.field "doneAt" (D.maybe decodeTime))

        model tasks projects =
            { emptyModel | tasks = tasks, projects = projects }
    in
    D.map2 model
        (D.field "tasks" (D.dict task) |> D.map (Dict.map (|>)))
        (D.field "projects" (D.list D.string))


decodeTime : D.Decoder Time.Posix
decodeTime =
    D.map Time.millisToPosix D.int


encodeTime : Time.Posix -> E.Value
encodeTime =
    E.int << Time.posixToMillis
