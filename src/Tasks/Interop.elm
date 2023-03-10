port module Tasks.Interop exposing (FromJs(..), load, log, save, subscribe)

import Dict
import Json.Decode as D
import Json.Decode.Extra as D
import Json.Encode as E
import Json.Encode.Extra as E
import Result.Extra as Result
import Tasks.Model exposing (Model, Task, TaskId, emptyModel)
import Tasks.Utils exposing (fmap)
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
    fmap performRequest Save


load : Cmd msg
load =
    performRequest Load


performRequest : ToJs -> Cmd msg
performRequest =
    fmap requestJs encodeRequest


subscribe : (FromJs -> msg) -> Sub msg
subscribe msg =
    fromJs <|
        \x ->
            D.decodeValue responseDecoder x
                |> Result.unpack Error identity
                |> msg


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
            D.map4 Task
                (D.field "text" D.string)
                (D.field "project" (D.maybe D.string))
                (D.field "createdAt" decodeTime)
                (D.field "doneAt" (D.maybe decodeTime))

        model tasks projects =
            { emptyModel | tasks = tasks, projects = projects }
    in
    D.map2 model
        (D.field "tasks" (D.dict task)
            |> D.map (Dict.map (\k v -> v k))
        )
        (D.field "projects" (D.list D.string))


decodeTime : D.Decoder Time.Posix
decodeTime =
    D.map Time.millisToPosix D.int


encodeTime : Time.Posix -> E.Value
encodeTime time =
    E.int <| Time.posixToMillis time
