module Tasks.Style exposing (Style, darkStyle, lightStyle, paddingScale)

import Element exposing (Color, rgb255)
import Tasks.Utils exposing (intModular)


type alias Style =
    { background : Color
    , taskBackground : Color
    , doneBackground : Color
    , buttonBackground : Color
    , textColor : Color
    , textSize : Int -> Int
    }


lightStyle : Style
lightStyle =
    { background = rgb255 225 225 225
    , taskBackground = rgb255 200 200 200
    , doneBackground = rgb255 100 200 100
    , buttonBackground = rgb255 180 180 180
    , textColor = rgb255 0 0 0
    , textSize = intModular 16 1.2
    }


darkStyle : Style
darkStyle =
    { background = rgb255 50 50 50
    , taskBackground = rgb255 75 75 75
    , doneBackground = rgb255 75 100 75
    , buttonBackground = rgb255 100 100 100
    , textColor = rgb255 255 255 255
    , textSize = intModular 16 1.2
    }


paddingScale : Int -> Int
paddingScale n =
    5 * n
