module Tasks.Task exposing
    ( SearchRule
    , Task
    , TaskId
    )

import Time


type alias TaskId =
    String


type alias SearchRule =
    { snippets : List String
    , tags : List String
    }


type alias Task =
    { text : String
    , tags : List String
    , project : Maybe String
    , createdAt : Time.Posix
    , doneAt : Maybe Time.Posix
    , id : TaskId
    }
