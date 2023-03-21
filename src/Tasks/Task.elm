module Tasks.Task exposing
    ( SearchRule
    , Task
    , TaskId
    , searchProject
    , emptySwarch
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
    , createdAt : Time.Posix
    , doneAt : Maybe Time.Posix
    , id : TaskId
    }

searchProject : String -> SearchRule
searchProject tag =
    { tags = ["#" ++ tag]
    , snippets = []
    }

emptySwarch : SearchRule
emptySwarch = { snippets = [], tags = [] }