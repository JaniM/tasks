module Tasks.Task exposing
    ( SearchRule
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


type alias Task =
    { -- Text of the task
      text : String

    -- Tags of the task
    , tags : List String

    -- When the task was created
    , createdAt : Time.Posix

    -- When the task was done
    , doneAt : Maybe Time.Posix

    -- When the task was picked to be done
    , pickedAt : Maybe Time.Posix

    -- Id of the task
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
    , createdAt = epoch
    , doneAt = Nothing
    , pickedAt = Nothing
    , id = ""
    }
