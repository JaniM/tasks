module Tasks.Task exposing
    ( Priority(..)
    , SearchRule
    , Task
    , TaskId
    , emptySwarch
    , emptyTask
    , searchProject
    )

import Tasks.Utils exposing (epoch)
import Time


type alias TaskId =
    String


type alias SearchRule =
    { snippets : List String
    , tags : List String
    }


type Priority
    = Low
    | Medium
    | High


type alias Task =
    { text : String
    , tags : List String
    , priority : Priority
    , createdAt : Time.Posix
    , doneAt : Maybe Time.Posix
    , pickedAt : Maybe Time.Posix
    , id : TaskId
    }


searchProject : String -> SearchRule
searchProject tag =
    { tags = [ "#" ++ tag ]
    , snippets = []
    }


emptySwarch : SearchRule
emptySwarch =
    { snippets = [], tags = [] }


emptyTask : Task
emptyTask =
    { text = ""
    , tags = []
    , priority = Medium
    , createdAt = epoch
    , doneAt = Nothing
    , pickedAt = Nothing
    , id = ""
    }
