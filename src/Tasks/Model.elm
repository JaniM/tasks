module Tasks.Model exposing (..)

import Prng.Uuid exposing (Uuid)
import Random.Pcg.Extended as Pcg
import Tasks.Style exposing (Style)


type alias Task =
    { text : String
    , project : Maybe String
    , id : Uuid
    }


type alias Model =
    { text : String
    , tasks : List Task
    , projects : List String
    , project : Maybe String
    , seed : Pcg.Seed
    , style : Style
    }


type Msg
    = SetText String
    | SubmitInput
    | RemoveTask Uuid
    | ToggleStyle
    | SetProject Bool String
    | DeleteProject String
    | Tabfill
    | LoadModel Model


emptyModel : Model
emptyModel =
    { text = ""
    , tasks = []
    , projects = []
    , seed = Pcg.initialSeed 0 []
    , style = Tasks.Style.darkStyle
    , project = Nothing
    }


isLoadModel msg =
    case msg of
        LoadModel _ ->
            True

        _ ->
            False
