port module Tasks.Interop exposing
    ( FromJs(..)
    , load
    , save
    , subscribe
    )

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra as E
import Result.Extra as Result
import Tasks.Model exposing (Model, StoredModel)
import Tasks.Task exposing (Task, TaskId)
import Time



-- port log : String -> Cmd msg


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


encodeModel : Model -> E.Value
encodeModel model =
    let
        task : Task -> D.Value
        task t =
            E.object
                [ ( "text", E.string t.text )
                , ( "tags", E.list E.string t.tags )
                , ( "createdAt", encodeTime t.createdAt )
                , ( "doneAt", E.maybe encodeTime t.doneAt )
                ]
    in
    E.object
        [ ( "tasks", E.dict identity task model.tasks )
        , ( "projects", E.list E.string model.projects )
        ]


decodeModel : D.Decoder StoredModel
decodeModel =
    let
        task : D.Decoder (TaskId -> Task)
        task =
            D.map4 Task
                (D.field "text" D.string)
                (D.field "tags" (D.list D.string))
                (D.field "createdAt" decodeTime)
                (D.field "doneAt" (D.maybe decodeTime))

        tasks : D.Decoder (Dict String Task)
        tasks =
            D.dict task
                |> D.field "tasks"
                |> D.map (Dict.map (\id taskf -> taskf id))

        projects : D.Decoder (List String)
        projects =
            D.list D.string
                |> D.field "projects"
    in
    D.map2 StoredModel tasks projects


decodeTime : D.Decoder Time.Posix
decodeTime =
    D.map Time.millisToPosix D.int


encodeTime : Time.Posix -> E.Value
encodeTime =
    E.int << Time.posixToMillis
