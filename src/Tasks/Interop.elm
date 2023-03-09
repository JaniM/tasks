port module Tasks.Interop exposing (FromJs(..), load, log, save, subscribe)

import Json.Decode as D
import Json.Decode.Extra as D
import Json.Encode as E
import Json.Encode.Extra as E
import Prng.Uuid
import Result.Extra as Result
import Tasks.Model exposing (Model, Task, emptyModel)
import Tasks.Utils exposing (fmap)


port log : String -> Cmd msg


port requestJs : E.Value -> Cmd msg


port fromJs : (D.Value -> msg) -> Sub msg


type ToJs
    = Save Model
    | Load


type FromJs
    = LoadModel Model
    | Error D.Error


save =
    fmap performRequest Save


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
        task t =
            E.object
                [ ( "text", E.string t.text )
                , ( "id", Prng.Uuid.encode t.id )
                , ( "project", E.maybe E.string t.project )
                ]
    in
    E.object
        [ ( "tasks", E.list task model.tasks )
        , ( "projects", E.list E.string model.projects )
        , ( "project", E.maybe E.string model.project )
        ]


decodeModel : D.Decoder Model
decodeModel =
    let
        task =
            D.map3 Task
                (D.field "text" D.string)
                (D.field "project" (D.maybe D.string))
                (D.field "id" Prng.Uuid.decoder)

        model tasks projects =
            { emptyModel | tasks = tasks, projects = projects }
    in
    D.map2 model
        (D.field "tasks" (D.list task))
        (D.field "projects" (D.list D.string))
